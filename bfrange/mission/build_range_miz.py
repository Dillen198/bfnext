# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""Build the Vector Strike training range mission (Caucasus) and its config.

    python build_range_miz.py [--donor rgw2008.miz] [--out DIR]

Writes, into DIR (default: E:/Saved Games/DCS/Missions/Vector/range):

    VS_Range_Caucasus.miz   the mission: sortie VSRANGE, no campaign content
    VSRANGE_RANGE           the matching bfrange config (JSON)

The mission is DERIVED from a Caucasus donor rather than written from
scratch, so everything a script can't easily produce correctly -- theatre,
weather, options, the dictionary, every airfield's warehouse -- is the
Mission Editor's own output. From the donor we keep only:

  * the blue Supercarrier (renamed CVN-72; its escorts, and every other
    group on the map, are removed),
  * the coalitions (CJTF Blue / CJTF Red),
  * the airfield warehouses, which get dynamic spawn, hot start, dynamic
    cargo and unlimited aircraft/munitions/fuel at the home fields.

and we add:

  * LHA-1 (Tarawa) next to the carrier,
  * deck client slots: F/A-18C x4, F-14B x2, F-14A x2 on CVN-72; AV-8B x2
    and UH-1H x2 on LHA-1 (dynamic spawn can't use a Supercarrier deck),
  * Combined Arms slots (tactical commander, JTAC/operator, observer),
  * the trigger zones the range config places things in,
  * the bfrange loader as the mission's init script.

The layout lives in layout.py: the whole theatre divided into SECTORS with
one job each (bombing ranges, fight areas, tanker tracks, the carrier's
operating area...). From that one table this script writes the trigger
zones, the range config (including its `sectors`) and the Mission Editor
drawings -- each sector outlined in its discipline's colour with a label,
plus a legend -- so the F10 map and the engine always agree. Ground sectors
were placed against DCS's own 1:1M chart of the theatre; the engine still
moves any target that lands in water to dry ground, but open the mission in
the Mission Editor once and eyeball the zones.
"""
import argparse
import copy
import json
import math
import os
import sys
import zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import luamiz  # noqa: E402

DEFAULT_DONOR = r"E:/Saved Games/DCS/Missions/Vector/caucasus/rgw2008.miz"
DEFAULT_OUT = r"E:/Saved Games/DCS/Missions/Vector/range"
SORTIE = "VSRANGE"
MIZ_NAME = "VS_Range_Caucasus.miz"

# ------------------------------------------------------------------ geometry

_p = json.load(open(os.path.join(HERE, "caucasus_proj.json")))
LAT0, LON0, DEG = _p["LAT0"], _p["LON0"], _p["deg"]


def _design(a, b):
    return [(a ** i) * (b ** j) for i in range(DEG + 1) for j in range(DEG + 1 - i)]


def ll2xz(lat, lon):
    """Caucasus lat/lon -> DCS x (north), z (east): a degree-4 fit over the
    terrain's own beacon table, good to a couple of metres."""
    a = _design(lat - LAT0, lon - LON0)
    return (sum(c * v for c, v in zip(_p["cx"], a)), sum(c * v for c, v in zip(_p["cz"], a)))


def xz2ll(x, z, lat=42.0, lon=42.0):
    for _ in range(60):
        cx, cz = ll2xz(lat, lon)
        dx, dz = x - cx, z - cz
        h = 1e-5
        x1, z1 = ll2xz(lat + h, lon)
        x2, z2 = ll2xz(lat, lon + h)
        a, b, c, d = (x1 - cx) / h, (x2 - cx) / h, (z1 - cz) / h, (z2 - cz) / h
        det = a * d - b * c
        lat += (d * dx - b * dz) / det
        lon += (-c * dx + a * dz) / det
        if abs(dx) < 0.5 and abs(dz) < 0.5:
            break
    return lat, lon


# ------------------------------------------------------------------ layout
#
# The theatre is divided into SECTORS, one job each, in layout.py. The zones,
# the range config and the F10 drawings are all generated from that table.

import layout  # noqa: E402
from layout import SECTORS, VAZIANI, MOZDOK, NM  # noqa: E402,F401

ZONES = layout.ZONES

CVN_POS = layout.centre("oparea")
LHA_POS = (41.62, 41.08)

BLUE_FIELDS = {22: "Batumi", 23: "Senaki-Kolkhi", 24: "Kobuleti", 25: "Kutaisi",
               29: "Tbilisi-Lochini", 30: "Soganlug", 31: "Vaziani"}
RED_FIELDS = {26: "Mineralnye Vody", 27: "Nalchik", 28: "Mozdok", 32: "Beslan",
              16: "Maykop-Khanskaya", 15: "Krymsk"}

# deck slots: (carrier, category, type, count, group name, callsign id, callsign name, fuel kg, task)
DECK = [
    ("CVN-72", "plane", "FA-18C_hornet", 2, "CVN Hornet 1", 1, "Enfield", 4900, "CAP"),
    ("CVN-72", "plane", "FA-18C_hornet", 2, "CVN Hornet 2", 2, "Springfield", 4900, "CAP"),
    ("CVN-72", "plane", "F-14B", 2, "CVN Tomcat B", 3, "Uzi", 7348, "CAP"),
    ("CVN-72", "plane", "F-14A-135-GR", 2, "CVN Tomcat A", 4, "Colt", 7348, "CAP"),
    ("LHA-1", "plane", "AV8BNA", 2, "LHA Harrier", 5, "Dodge", 3519, "CAS"),
    ("LHA-1", "helicopter", "UH-1H", 2, "LHA Huey", 6, "Ford", 631, "Transport"),
]

BRIEFING = """VECTOR STRIKE TRAINING RANGE - CAUCASUS

A practice server. No campaign, no lives, no points: every slot is open and
everything you fly is graded. Results, debriefs, the greenie board and the
spawner are at https://range.vectorstrike.org

THE WHOLE MAP IS THE RANGE
Every coloured area on the F10 map is a SECTOR with one job; its label says
what is in it. F10 > Range > Sectors lists them nearest first with bearing and
range, and you are told what a sector is for when you fly into it.
  amber  air-to-ground     yellow  tactical / CAS    red    threat / SEAD
  tan    CA gunnery        lime    helicopter        cyan   air-to-air
  blue   BVR               pink    duels             green  tanker tracks
  white  carrier ops       violet  anti-ship

BLUE - Georgia
  R-1 SAMGORI        bomb circle, strafe pit (east of Vaziani)
  R-2 IORI           tactical array laser 1688, convoy, JDAM, JTAC Axeman 133.0
  R-3 TETRI TSKARO   SA-8 threat range (radar on, weapons hold)
  G-1 LILO           Combined Arms gunnery lane
  H-1 VAZIANI, H-2 TSKALTUBO (Kutaisi, mountains)   helicopter courses
  MOA KAKHETI        BFM over the mountains
  AR-4 TEXACO 2      KC-135 boom, 54Y, 254.0
RED - Russia
  R-11 STEPNOYE      bomb circle, strafe pit (north of Mozdok)
  R-12 KARST         tactical array laser 1511, convoy, JDAM, JTAC Topor 124.5
  R-13 ACHIKULAK     SA-8 threat range
  R-14 KUBAN         bombing and tactical array 1512 for Maykop / Krymsk
  G-11 STARODUB      Combined Arms gunnery lane
  H-11 MOZDOK, H-12 NALCHIK (foothills)   helicopter courses
  MOA NOGAI          BFM over the steppe
  AR-5 / AR-6        IL-78M, 59Y 124.0 (north) / 58Y 123.0 (west, over the sea)
BLACK SEA - everyone
  CV OPAREA   CVN-72 TACAN 72X ICLS 11 Link-4 336.0, LHA-1 71X,
              recovery tanker A-6E 63Y 261.0. Every pass graded.
  W-1 / W-2   BFM        W-3 BVR        W-4 DUEL (blue meets red)
  AS-1        shipping for anti-ship weapons
  AR-1 TEXACO KC-135 51Y 251.0, AR-2 ARCO KC-135MPRS 52Y 252.0,
  AR-3 SHELL KC-130 53Y 253.0 (helicopters too)

The MISSILE TRAINER removes any missile that would kill you and tells you
so; guns are real. Chat: -range trainer on|off
"""

# ------------------------------------------------------------------ geometry of sectors


def sector_ring(shape):
    """A sector's outline in DCS x/z (the same numbers the drawing uses)."""
    kind = shape[0]
    if kind == "rect":
        _, c, w, h = shape
        x, z = ll2xz(*c)
        return [(x + h / 2, z - w / 2), (x + h / 2, z + w / 2), (x - h / 2, z + w / 2), (x - h / 2, z - w / 2)]
    if kind == "circle":
        _, c, r = shape
        x, z = ll2xz(*c)
        return [(x + r * math.cos(a), z + r * math.sin(a)) for a in (i * math.pi / 24 for i in range(48))]
    if kind == "track":
        _, s, hdg, leg, width = shape
        x, z = ll2xz(*s)
        h = math.radians(hdg)
        fx, fz = math.cos(h), math.sin(h)
        rx, rz = math.cos(h + math.pi / 2), math.sin(h + math.pi / 2)
        r = width / 2
        ends = ((x + fx * leg, z + fz * leg, -math.pi / 2), (x, z, math.pi / 2))
        pts = []
        for cx, cz, a0 in ends:
            for i in range(13):
                a = a0 + math.pi * i / 12
                pts.append((cx + r * (math.sin(a) * rx + math.cos(a) * fx), cz + r * (math.sin(a) * rz + math.cos(a) * fz)))
        return pts
    raise ValueError(kind)


def sector_label_at(shape):
    """Where a sector's label goes: its north-west corner (DCS draws a text
    box down and to the right of its position). A tanker track's label hangs
    below the track instead, or it would cover the track it names."""
    ring = sector_ring(shape)
    if shape[0] == "circle":
        _, c, r = shape
        x, z = ll2xz(*c)
        return x + r * 0.71, z - r * 0.71
    if shape[0] == "track":
        return min(p[0] for p in ring), min(p[1] for p in ring)
    return max(p[0] for p in ring), min(p[1] for p in ring)


# ------------------------------------------------------------------ config

def cfg_loc(zone):
    return {"zone": zone}


def cfg_ll(ll):
    return {"lat": round(ll[0], 5), "lon": round(ll[1], 5)}


def conventional(p, sid, name, side, country):
    """A bomb circle and a strafe pit, the strafe run-in from the east."""
    return [
        {"id": f"{sid}-bomb", "name": f"{name} - Bomb Circle", "kind": "bomb_circle",
         "loc": cfg_loc(f"{p}-BOMB"), "rings_m": [25, 50, 100, 150], "side": side, "country": country,
         "targets": [{"typ": "Container red 1", "category": "static", "name": f"Circle {name.split()[0]}"}],
         "note": "Unguided and guided bombs, rockets. Any run-in heading."},
        {"id": f"{sid}-strafe", "name": f"{name} - Strafe Pit", "kind": "strafe_pit",
         "loc": cfg_loc(f"{p}-STRAFE"), "heading_deg": 270, "side": side, "country": country,
         "strafe": {"box_length_m": 3000, "box_width_m": 300, "foul_line_m": 610, "max_alt_agl_m": 914},
         "targets": [{"typ": "Container white", "category": "static", "y": -15},
                     {"typ": "Container white", "category": "static", "y": 15}],
         "note": "Foul line 2000 ft. Run in from the east, heading 270."},
    ]


def tactical_array(p, sid, name, side, country, laser):
    return {"id": f"{sid}-array", "name": f"{name} - Tactical Array", "kind": "tactical_array",
            "loc": cfg_loc(f"{p}-ARRAY"), "heading_deg": 45, "laser_code": laser, "rings_m": [25, 50],
            "side": side, "country": country, "respawn_s": 300,
            "targets": [{"typ": "T-72B", "category": "vehicle", "x": 0, "y": 0},
                        {"typ": "T-72B", "category": "vehicle", "x": 40, "y": 20},
                        {"typ": "BMP-2", "category": "vehicle", "x": -40, "y": 25},
                        {"typ": "BMP-2", "category": "vehicle", "x": 30, "y": -35},
                        {"typ": "Ural-375", "category": "vehicle", "x": -60, "y": -20},
                        {"typ": "Ural-375", "category": "vehicle", "x": 70, "y": -10}],
            "note": f"AI laser on the first live target, code {laser}."}


def tactical(p, sid, name, side, country, laser):
    """Tactical array, a moving convoy and coordinate targets."""
    return [
        tactical_array(p, sid, name, side, country, laser),
        {"id": f"{sid}-convoy", "name": f"{name} - Moving Convoy", "kind": "convoy",
         "loc": cfg_loc(f"{p}-CONVOY"), "side": side, "country": country, "respawn_s": 600,
         "targets": [{"typ": "Ural-375", "category": "vehicle"},
                     {"typ": "Ural-375", "category": "vehicle", "x": -30},
                     {"typ": "BTR-80", "category": "vehicle", "x": -60},
                     {"typ": "Ural-375", "category": "vehicle", "x": -90}],
         "route": {"points": [cfg_loc(f"{p}-CONVOY-END")], "speed_kts": 20, "on_road": False}},
        {"id": f"{sid}-jdam", "name": f"{name} - Coordinate Targets", "kind": "coord_target",
         "loc": cfg_loc(f"{p}-JDAM"), "side": side, "country": country, "respawn_s": 900,
         "targets": [{"typ": "Hangar A", "category": "static", "name": "Hangar"},
                     {"typ": "Garage A", "category": "static", "x": 120, "y": 60, "name": "Garage"},
                     {"typ": ".Command Center", "category": "static", "x": -150, "y": 80, "name": "Command post"}],
         "note": "JDAM / JSOW / coordinate weapons. Coordinates: F10 > Range > Air-to-Ground > Stations."},
    ]


def threat(p, sid, name, side, country):
    return [{"id": f"{sid}-sam", "name": f"{name} - SA-8 Threat", "kind": "sam_site",
             "loc": cfg_loc(f"{p}-SAM"), "side": side, "country": country,
             "targets": [{"typ": "Osa 9A33 ln", "category": "vehicle"}], "weapons_free": False, "locked": True,
             "note": "Radar on, weapons hold. Instructors can go weapons free; the missile trainer protects you."}]


def gunnery(p, sid, name, side, country):
    return [{"id": f"{sid}-lane", "name": f"{name} - CA Gunnery Lane", "kind": "gunnery_lane",
             "loc": cfg_loc(f"{p}-LANE"), "heading_deg": 0, "side": side, "country": country, "respawn_s": 120,
             "targets": [{"typ": "T-55", "category": "vehicle", "x": 800},
                         {"typ": "T-55", "category": "vehicle", "x": 1200, "y": 150},
                         {"typ": "BMP-1", "category": "vehicle", "x": 1600, "y": -120}]}]


def helo_cfg(p, sid, name):
    return {
        "pads": [
            {"id": f"{sid}-pad", "name": f"{name} - Precision Pad", "loc": cfg_loc(f"{p}-PAD"), "drill": "precision", "perfect_m": 3},
            {"id": f"{sid}-confined", "name": f"{name} - Confined Area LZ", "loc": cfg_loc(f"{p}-CONFINED"), "drill": "confined", "perfect_m": 5},
            {"id": f"{sid}-pinnacle", "name": f"{name} - Pinnacle", "loc": cfg_loc(f"{p}-PINNACLE"), "drill": "pinnacle", "perfect_m": 4},
        ],
        "sling": [
            {"id": f"{sid}-sling-1", "name": f"{name} - Sling Course", "pickup": cfg_loc(f"{p}-SLING1-PICKUP"), "dropzone": cfg_loc(f"{p}-SLING1-DZ"), "cargo_type": "uh1h_cargo", "mass_kg": 800, "perfect_m": 5},
            {"id": f"{sid}-sling-2", "name": f"{name} - Heavy Lift", "pickup": cfg_loc(f"{p}-SLING2-PICKUP"), "dropzone": cfg_loc(f"{p}-SLING2-DZ"), "cargo_type": "container_cargo", "mass_kg": 3500, "perfect_m": 8},
        ],
        "troops": [
            {"id": f"{sid}-troops", "name": f"{name} - Air Assault", "pickup": cfg_loc(f"{p}-TROOPS-PICKUP"), "lz": cfg_loc(f"{p}-TROOPS-LZ"), "troops": 8, "radius_m": 60},
        ],
    }


# tanker per AR sector: id, callsign, number, type, alt ft, MHz, TACAN channel, morse, side, country
TANKERS = {
    "ar-1": ("texaco", "Texaco", 1, "KC-135", 25000, 251.0, 51, "TEX", "blue", "CJTF Blue"),
    "ar-2": ("arco", "Arco", 1, "KC135MPRS", 21000, 252.0, 52, "ARC", "blue", "CJTF Blue"),
    "ar-3": ("shell", "Shell", 1, "KC130", 14000, 253.0, 53, "SHL", "blue", "CJTF Blue"),
    "ar-4": ("texaco2", "Texaco", 2, "KC-135", 24000, 254.0, 54, "TX2", "blue", "CJTF Blue"),
    "ar-5": ("ilyusha", "Texaco", 9, "IL-78M", 23000, 124.0, 59, "ILR", "red", "CJTF Red"),
    "ar-6": ("ilyusha-west", "Texaco", 8, "IL-78M", 22000, 123.0, 58, "ILW", "red", "CJTF Red"),
}

ARENA_FLOOR_FT = {"moa-kakheti": 8000}


def sector_cfg(s):
    """The engine's copy of a sector: the same outline the drawing uses."""
    shape = s["shape"]
    if shape[0] == "circle":
        _, c, r = shape
        geo = {"circle": {"lat": round(c[0], 5), "lon": round(c[1], 5), "radius_m": round(r, 1)}}
    elif shape[0] == "track":
        _, st, hdg, leg, width = shape
        geo = {"track": {"lat": round(st[0], 5), "lon": round(st[1], 5), "heading_deg": hdg,
                         "leg_m": round(leg, 1), "width_m": round(width, 1)}}
    else:
        pts = [xz2ll(x, z, *shape[1]) for x, z in sector_ring(shape)]
        geo = {"polygon": [{"lat": round(a, 5), "lon": round(o, 5)} for a, o in pts]}
    return {"id": s["id"], "name": s["name"], "kind": s["kind"], "side": s["side"],
            "purpose": s["purpose"], "shape": geo}


def build_config():
    red, blue = ("red", "CJTF Red"), ("blue", "CJTF Blue")
    stations = (
        # blue pilots: red targets
        conventional("R1", "r1", "R-1 Samgori", *red)
        + tactical("R2", "r2", "R-2 Iori", *red, 1688)
        + threat("R3", "r3", "R-3 Tetri Tskaro", *red)
        + gunnery("G1", "g1", "G-1 Lilo", *red)
        # red pilots: blue targets
        + conventional("R11", "r11", "R-11 Stepnoye", *blue)
        + tactical("R12", "r12", "R-12 Karst", *blue, 1511)
        + threat("R13", "r13", "R-13 Achikulak", *blue)
        + gunnery("G11", "g11", "G-11 Starodub", *blue)
        + conventional("R14", "r14", "R-14 Kuban", *blue)
        + [tactical_array("R14", "r14", "R-14 Kuban", *blue, 1512)]
        + [{"id": "as1-ships", "name": "AS-1 Shipping - Merchant Ships", "kind": "ship_target",
            "loc": cfg_ll(layout.at("as-1", -3300, 21600)), "side": "red", "country": "CJTF Red", "respawn_s": 900,
            "targets": [{"typ": "Dry-cargo ship-1", "category": "ship"},
                        {"typ": "ELNYA", "category": "ship", "y": 600}],
            "route": {"points": [cfg_ll(layout.at("as-1", -3300, -23000))], "speed_kts": 12},
            "note": "Undefended shipping for anti-ship missiles and bombs."}]
    )
    helos = [helo_cfg("H1", "h1", "H-1 Vaziani"), helo_cfg("H2", "h2", "H-2 Tskaltubo"),
             helo_cfg("H11", "h11", "H-11 Mozdok"), helo_cfg("H12", "h12", "H-12 Nalchik")]
    helo = {k: sum((h[k] for h in helos), []) for k in helos[0]}
    tankers = []
    for s in SECTORS:
        if s["id"] in TANKERS:
            i, cs, n, typ, alt, mhz, ch, morse, side, country = TANKERS[s["id"]]
            _, start, hdg, leg, _ = s["shape"]
            tankers.append({"id": i, "callsign": cs, "callsign_number": n, "typ": typ, "side": side, "country": country,
                            "loc": cfg_ll(start), "heading_deg": hdg, "leg_nm": round(leg / NM, 1), "alt_ft": alt,
                            "freq_mhz": mhz, "tacan": {"channel": ch, "band": "Y", "morse": morse}})
    arenas = []
    for s in SECTORS:
        if s["kind"] not in ("air_to_air", "bvr", "duel"):
            continue
        shape = s["shape"]
        radius = shape[2] if shape[0] == "circle" else max(shape[2], shape[3]) / 2
        arenas.append({"id": s["id"], "name": s["name"], "loc": cfg_ll(shape[1]), "radius_nm": round(radius / NM, 1),
                       "duels": True, "floor_ft": ARENA_FLOOR_FT.get(s["id"], 5000)})
    return {
        "netidx_base": "/local/fowl/range",
        "name": "Vector Strike Range",
        "welcome": "Welcome to the Vector Strike Range. The coloured sectors on the F10 map each have one job; F10 > Range > Sectors lists them. The missile trainer is ON. Debriefs and spawning: range.vectorstrike.org",
        "sectors": [sector_cfg(s) for s in SECTORS],
        "stations": stations,
        "tankers": tankers,
        "carriers": [
            {"id": "cvn72", "unit_name": "CVN-72", "name": "CVN-72 Abraham Lincoln", "kind": "nimitz",
             "tacan": {"channel": 72, "band": "X", "morse": "ABL"}, "icls_channel": 11, "acls": True,
             "link4_mhz": 336.0, "tower_mhz": 305.0, "wind_over_deck_kts": 27,
             "op_center": cfg_ll(CVN_POS), "op_radius_nm": 20,
             "recovery_tanker": {"id": "cvn72-tanker", "callsign": "Arco", "callsign_number": 3, "typ": "A-6E",
                                 "loc": cfg_ll(CVN_POS), "heading_deg": 0, "leg_nm": 10, "alt_ft": 6000,
                                 "freq_mhz": 261.0, "tacan": {"channel": 63, "band": "Y", "morse": "A6E"}},
             "plane_guard": "SH-60B"},
            {"id": "lha1", "unit_name": "LHA-1", "name": "LHA-1 Tarawa", "kind": "tarawa",
             "tacan": {"channel": 71, "band": "X", "morse": "LHA"}, "icls_channel": 12, "acls": False,
             "tower_mhz": 306.0, "wind_over_deck_kts": 20, "op_center": cfg_ll(LHA_POS), "op_radius_nm": 15},
        ],
        "air_to_air": {
            "missile_trainer": {"enabled": True, "kill_radius_m": 200, "big_kill_radius_m": 500,
                                "big_warhead_kg": 50, "launch_alerts": True, "protect_ai": False, "max_tracked": 64},
            "arenas": arenas,
            "duels": True,
            "gun_kill_hits": 10,
        },
        "helo": helo,
        "jtacs": [
            {"id": "axeman", "callsign": "Axeman", "station": "r2-array", "loc": cfg_loc("R2-ARRAY"),
             "laser_code": 1688, "freq_mhz": 133.0, "typ": "MQ-9 Reaper", "friendlies_m": 600},
            {"id": "topor", "callsign": "Topor", "station": "r12-array", "loc": cfg_loc("R12-ARRAY"),
             "laser_code": 1511, "freq_mhz": 124.5, "typ": "MQ-9 Reaper", "friendlies_m": 600},
        ],
        "spawn": {"max_active_per_player": 3, "max_ai_units": 60, "cooldown_s": 20, "despawn_after_s": 3600,
                  "despawn_on_leave": True, "instructor_only": ["sam_site", "naval_group"], "instructors": []},
        "in_game_results": True,
        "message_s": 20,
        "record_tracks": True,
    }


# ------------------------------------------------------------------ drawings

def _rgba(rgb, alpha):
    return "0x%06x%02x" % (rgb, alpha)


def _wrap(text, width=46):
    out, line = [], ""
    for word in text.split():
        if line and len(line) + 1 + len(word) > width:
            out.append(line)
            line = word
        else:
            line = f"{line} {word}".strip()
    if line:
        out.append(line)
    return out


LAYER = {"blue": "Blue", "red": "Red", "all": "Common"}


def _textbox(name, layer, at, text, rgb, size=12, alpha=0xd0):
    return {"visible": True, "layerName": layer, "primitiveType": "TextBox",
            "mapX": at[0], "mapY": at[1], "name": name, "text": text,
            "font": "DejaVuLGCSansCondensed.ttf", "fontSize": size, "borderThickness": 2,
            "colorString": _rgba(rgb, 0xff), "fillColorString": "0x101418%02x" % alpha, "angle": 0}


def sector_drawings():
    """Mission Editor drawings for every sector: its outline, filled lightly in
    its discipline's colour, and a label at its north-west corner. A side's
    sectors go on that side's layer (only that coalition sees them), shared
    ones on Common. Formats copied from Mission Editor output.

    Returns {layer name: [objects]}."""
    layers = {"Blue": [], "Red": [], "Common": []}
    by_id = {s["id"]: s for s in SECTORS}
    for s in SECTORS:
        rgb = layout.COLOURS[s["kind"]]
        shape = s["shape"]
        layer = LAYER[s["side"]]
        style = "dash" if s["kind"] == "aar" else "solid"
        common = {"visible": True, "layerName": layer, "primitiveType": "Polygon",
                  "colorString": _rgba(rgb, 0xff), "fillColorString": _rgba(rgb, 0x1a if s["side"] == "all" else 0x2c),
                  "thickness": 6 if s["kind"] == "aar" else 8, "style": style, "angle": 0}
        if shape[0] == "circle":
            _, c, r = shape
            x, z = ll2xz(*c)
            layers[layer].append({**common, "polygonMode": "circle", "radius": r, "mapX": x, "mapY": z, "name": s["name"]})
        else:
            ring = sector_ring(shape)
            cx = sum(p[0] for p in ring) / len(ring)
            cz = sum(p[1] for p in ring) / len(ring)
            layers[layer].append({**common, "polygonMode": "free", "mapX": cx, "mapY": cz, "name": s["name"],
                                  # distinct vertices only: DCS closes the ring
                                  # itself, and a repeated first point kills the fill
                                  "points": [{"x": px - cx, "y": pz - cz} for px, pz in ring]})
        if s["kind"] in layout.SMALL_KINDS:
            text = f"{s['name']}\n{layout.KIND_LABEL[s['kind']]}"
            size = 11
        else:
            side = {"blue": "BLUE", "red": "RED", "all": "ALL"}[s["side"]]
            text = "\n".join([s["name"], f"{layout.KIND_LABEL[s['kind']]} - {side}"] + _wrap(s["purpose"]))
            size = 12
        layers[layer].append(_textbox(f"{s['name']} label", layer, sector_label_at(shape), text, rgb, size))
    for side, sm in layout.SUMMARIES.items():
        lines = [sm["title"]]
        for sid in sm["ids"]:
            sec = by_id[sid]
            lines.append(f"{sec['name']}: " + sec["purpose"])
        lines.append("F10 > Range > Sectors: bearing and range to each")
        wrapped = []
        for line in lines:
            w = _wrap(line, 60)
            wrapped += [w[0]] + ["    " + x for x in w[1:]]
        layers[LAYER[side]].append(_textbox(sm["title"], LAYER[side], ll2xz(*sm["at"]), "\n".join(wrapped),
                                            0xF2F2F2, 12, 0xe0))
    legend = ["VECTOR STRIKE RANGE - SECTORS",
              "Every coloured area has one job:",
              "amber  air-to-ground     yellow  tactical / CAS",
              "red    threat / SEAD     tan     CA gunnery",
              "lime   helicopter        cyan    air-to-air",
              "blue   BVR               pink    duels",
              "green  tanker tracks     white   carrier ops",
              "violet anti-ship",
              "R- range   G- gunnery   H- helicopter   W- over water",
              "MOA fight area   AR- tanker track   AS- anti-ship",
              "You see your side's sectors and the shared ones.",
              "F10 > Range > Sectors: bearing and range to each"]
    layers["Common"].append(_textbox("Range legend", "Common", ll2xz(*layout.LEGEND_AT), "\n".join(legend), 0xF2F2F2, 12, 0xe0))
    return layers


# ------------------------------------------------------------------ mission

T = luamiz.LuaTable


def lt(d):
    """dict/list -> LuaTable (lists become 1-based arrays)."""
    if isinstance(d, luamiz.LuaTable):
        return d
    if isinstance(d, dict):
        return T({k: lt(v) for k, v in d.items()})
    if isinstance(d, list):
        return T({i + 1: lt(v) for i, v in enumerate(d)})
    return d


def combo(tasks):
    return {"id": "ComboTask", "params": {"tasks": {i + 1: t for i, t in enumerate(tasks)}}}


def wrapped(n, action):
    return {"number": n, "auto": False, "id": "WrappedAction", "enabled": True, "params": {"action": action}}


def build_mission(m, dictionary, next_ids):
    gid, uid = next_ids
    # --- clock and weather: a clear summer morning
    m["date"] = T({"Year": 2024, "Month": 6, "Day": 15})
    m["start_time"] = 8 * 3600
    w = m["weather"]
    w["clouds"]["density"] = 0
    w["clouds"]["iprecptns"] = 0
    w["enable_fog"] = False
    w["visibility"]["distance"] = 80000

    # --- strip every group except the blue Supercarrier
    cvn = None
    for side in ("blue", "red", "neutrals"):
        for c in m["coalition"][side]["country"].values():
            for cat in ("plane", "helicopter", "vehicle", "ship", "static"):
                if cat not in c:
                    continue
                keep = []
                for g in c[cat]["group"].values():
                    if side == "blue" and cat == "ship":
                        for u in g["units"].values():
                            if str(u.get("type", "")).startswith("CVN_7") and cvn is None:
                                g = copy.deepcopy(g)
                                g["units"] = T({1: u})
                                cvn = g
                                keep.append(g)
                                break
                c[cat]["group"] = T({i + 1: g for i, g in enumerate(keep)})
                if not keep:
                    del c[cat]
    if cvn is None:
        raise SystemExit("donor has no blue CVN_7x carrier")

    # --- the carrier: renamed, moved to the range's op area, immortal, holds fire
    cx, cz = ll2xz(*CVN_POS)
    u = cvn["units"][1]
    u["name"] = "CVN-72"
    u["x"], u["y"] = cx, cz
    u["heading"] = 0
    u["allowLso"] = True
    u["allowAirboss"] = True
    cvn["name"] = "CVN-72 Group"
    cvn["x"], cvn["y"] = cx, cz
    p1 = cvn["route"]["points"][1]
    p1["x"], p1["y"] = cx, cz
    p1["speed"] = 0
    p1["task"] = lt(combo([
        wrapped(1, {"id": "SetImmortal", "params": {"value": True}}),
        wrapped(2, {"id": "Option", "params": {"value": 4, "name": 0}}),  # ROE weapon hold
    ]))
    cvn_uid = u["unitId"]

    # --- LHA-1
    lx, lz = ll2xz(*LHA_POS)
    lha_uid = uid
    uid += 1
    lha = lt({
        "visible": False, "tasks": {}, "uncontrollable": False, "hidden": False,
        "groupId": gid, "name": "LHA-1 Group", "x": lx, "y": lz, "start_time": 0,
        "route": {"points": [{"alt": 0, "type": "Turning Point", "ETA": 0, "alt_type": "BARO", "x": lx, "y": lz,
                              "formation_template": "", "speed_locked": True, "ETA_locked": True, "speed": 0,
                              "action": "Turning Point",
                              "task": combo([wrapped(1, {"id": "SetImmortal", "params": {"value": True}}),
                                             wrapped(2, {"id": "Option", "params": {"value": 4, "name": 0}})])}]},
        "units": [{"type": "LHA_Tarawa", "unitId": lha_uid, "skill": "Excellent", "x": lx, "y": lz,
                   "name": "LHA-1", "heading": 0, "modulation": 0, "frequency": 127500000}],
    })
    gid += 1
    blue = next(c for c in m["coalition"]["blue"]["country"].values() if c["name"] == "CJTF Blue")
    ships = blue["ship"]["group"]
    ships[len(ships) + 1] = lha

    # --- deck client slots
    decks = {"CVN-72": (cvn_uid, cx, cz), "LHA-1": (lha_uid, lx, lz)}
    parking = {"CVN-72": 1, "LHA-1": 1}
    for carrier, cat, typ, count, gname, cs_id, cs_name, fuel, task in DECK:
        link, x, z = decks[carrier]
        units = []
        for i in range(count):
            units.append({
                "alt": 0, "alt_type": "BARO", "skill": "Client", "speed": 138.88888888889,
                "type": typ, "unitId": uid, "x": x, "y": z, "psi": 0, "heading": 0,
                "name": f"{gname}-{i + 1}", "parking": str(parking[carrier]),
                "payload": {"pylons": {}, "fuel": fuel, "flare": 60, "chaff": 60, "gun": 100},
                "callsign": {1: cs_id, 2: 1, 3: i + 1, "name": f"{cs_name}1{i + 1}"},
                "onboard_num": f"{300 + cs_id * 10 + i}",
            })
            uid += 1
            parking[carrier] += 1
        grp = lt({
            "dynSpawnTemplate": False, "modulation": 0, "tasks": {}, "radioSet": False, "task": task,
            "uncontrolled": False, "hidden": False, "groupId": gid, "name": gname,
            "x": x, "y": z, "start_time": 0, "frequency": 305 if carrier == "CVN-72" else 306,
            "communication": True,
            "route": {"points": [{"alt": 0, "action": "From Parking Area", "alt_type": "BARO",
                                  "linkUnit": link, "helipadId": link, "properties": {"addopt": {}},
                                  "speed": 138.88888888889, "type": "TakeOffParking", "ETA": 0,
                                  "ETA_locked": True, "x": x, "y": z, "speed_locked": True,
                                  "formation_template": "", "task": combo([])}]},
            "units": units,
        })
        gid += 1
        if cat not in blue:
            blue[cat] = T({"group": T()})
        grps = blue[cat]["group"]
        grps[len(grps) + 1] = grp

    # --- Combined Arms slots
    gc = m["groundControl"]
    gc["isPilotControlVehicles"] = False
    gc["roles"] = lt({
        "artillery_commander": {"blue": 2, "neutrals": 0, "red": 2},
        "instructor": {"blue": 1, "neutrals": 0, "red": 1},
        "forward_observer": {"blue": 2, "neutrals": 0, "red": 2},
        "observer": {"blue": 2, "neutrals": 0, "red": 2},
    })

    # --- zones: coloured like the sector they belong to
    zones = T()
    for i, (name, (lat, lon), radius) in enumerate(ZONES, start=1):
        x, z = ll2xz(lat, lon)
        home = next((sec for sec in SECTORS if sec["id"].replace("-", "").upper() == name.split("-")[0]), None)
        rgb = layout.COLOURS[home["kind"]] if home else 0xFFFF00
        zones[i] = lt({
            "radius": radius, "zoneId": i,
            "color": {1: ((rgb >> 16) & 255) / 255, 2: ((rgb >> 8) & 255) / 255, 3: (rgb & 255) / 255, 4: 0.15},
            "properties": {}, "hidden": False, "x": x, "y": z,
            "name": name, "heading": 0, "type": 0,
        })
    m["triggers"]["zones"] = zones

    # --- no triggers: the engine is loaded by the init script
    m["trig"] = lt({"custom": {}, "customStartup": {}, "events": {}, "func": {}, "flag": {},
                    "conditions": {}, "actions": {}, "funcStartup": {}})
    m["trigrules"] = T()
    # --- F10 drawings: the sectors, their labels and a legend, on the layer
    # every role sees (Common); the donor's campaign drawings go
    drawn = sector_drawings()
    for layer in m["drawings"]["layers"].values():
        layer["objects"] = lt(drawn.get(layer.get("name"), []))
        if layer.get("name") in drawn:
            layer["visible"] = True

    # --- dictionary: sortie + briefings
    dictionary[m["sortie"]] = SORTIE
    dictionary[m["descriptionText"]] = BRIEFING
    dictionary[m["descriptionBlueTask"]] = "Blue: home fields Batumi, Kobuleti, Senaki, Kutaisi, Tbilisi-Lochini, Soganlug, Vaziani (dynamic spawn, hot start, every airframe); CVN-72 and LHA-1 deck slots. Your ground ranges are R-1 to R-3 and G-1 east and south of Tbilisi, helicopter areas H-1 (Vaziani) and H-2 (Kutaisi). The Black Sea sectors are shared."
    dictionary[m["descriptionRedTask"]] = "Red: home fields Mozdok, Beslan, Nalchik, Mineralnye Vody, Maykop, Krymsk (dynamic spawn, hot start, every airframe). Your ground ranges are R-11 to R-13 and G-11 north of Mozdok and R-14 on the Kuban for Maykop and Krymsk, helicopter areas H-11 (Mozdok) and H-12 (Nalchik). IL-78M tankers AR-5 and AR-6. The Black Sea sectors are shared."
    dictionary[m["descriptionNeutralsTask"]] = ""
    # open on the whole theatre
    m["map"] = T({"centerX": -200000, "centerY": 560000, "zoom": 1400000})
    return (gid, uid), cvn_uid, lha_uid


def build_warehouses(wh, cvn_uid, lha_uid):
    for key, a in wh["airports"].items():
        side = "BLUE" if key in BLUE_FIELDS else "RED" if key in RED_FIELDS else "NEUTRAL"
        a["coalition"] = side
        home = side != "NEUTRAL"
        a["dynamicSpawn"] = home
        a["allowHotStart"] = home
        a["dynamicCargo"] = home
        a["unlimitedAircrafts"] = True
        a["unlimitedMunitions"] = True
        a["unlimitedFuel"] = True
    ws = wh["warehouses"]
    keep = T()
    if cvn_uid in ws:
        e = ws[cvn_uid]
        e["unlimitedAircrafts"] = True
        e["unlimitedMunitions"] = True
        e["unlimitedFuel"] = True
        keep[cvn_uid] = e
        e2 = copy.deepcopy(e)
        keep[lha_uid] = e2
    wh["warehouses"] = keep


LOADER = """-- Vector Strike training range loader (bfrange). Needs require/package/lfs
-- in the mission environment, the same desanitisation bflib uses.
package.cpath = package.cpath .. ";" .. lfs.writedir() .. "\\\\Scripts\\\\?.dll"
local bfrange = require("bfrange")
bfrange.initMiz()
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--donor", default=DEFAULT_DONOR)
    ap.add_argument("--out", default=DEFAULT_OUT)
    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    zin = zipfile.ZipFile(args.donor)
    files = {n: zin.read(n) for n in zin.namelist()}

    mv, m = luamiz.parse_mission(files["mission"].decode("utf-8", errors="replace"))
    dv, dictionary = luamiz.parse_mission(files["l10n/DEFAULT/dictionary"].decode("utf-8", errors="replace"))
    wv, wh = luamiz.parse_mission(files["warehouses"].decode("utf-8", errors="replace"))
    rv, res = luamiz.parse_mission(files["l10n/DEFAULT/mapResource"].decode("utf-8", errors="replace"))

    # fresh ids above anything the donor used
    max_gid = max_uid = 0
    for side in ("blue", "red", "neutrals"):
        for c in m["coalition"][side]["country"].values():
            for cat in ("plane", "helicopter", "vehicle", "ship", "static"):
                for g in c.get(cat, {}).get("group", {}).values():
                    max_gid = max(max_gid, g.get("groupId", 0))
                    for u in g["units"].values():
                        max_uid = max(max_uid, u.get("unitId", 0))
    (gid, uid), cvn_uid, lha_uid = build_mission(m, dictionary, (max_gid + 1, max_uid + 1))
    build_warehouses(wh, cvn_uid, lha_uid)

    # init script: the bfrange loader replaces bflib's
    init_key = m["initScriptFile"]
    old_init = res.get(init_key)
    res[init_key] = "bfrange_mizinit.lua"
    if old_init:
        files.pop("l10n/DEFAULT/" + old_init, None)
    files["l10n/DEFAULT/bfrange_mizinit.lua"] = LOADER.encode("utf-8")

    files["mission"] = luamiz.dump_mission(mv, m).encode("utf-8")
    files["l10n/DEFAULT/dictionary"] = luamiz.dump_mission(dv, dictionary).encode("utf-8")
    files["warehouses"] = luamiz.dump_mission(wv, wh).encode("utf-8")
    files["l10n/DEFAULT/mapResource"] = luamiz.dump_mission(rv, res).encode("utf-8")

    out_miz = os.path.join(args.out, MIZ_NAME)
    tmp = out_miz + ".tmp"
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        # DCS wants `mission` first, as the ME writes it
        order = ["mission"] + [n for n in files if n != "mission"]
        for n in order:
            zout.writestr(n, files[n])
    os.replace(tmp, out_miz)

    cfg = {"_copyright": "Copyright (c) 2026 Dillen Weerasinghe. All rights reserved. Proprietary; see the repository NOTICE file.",
           **build_config()}
    cfg_path = os.path.join(args.out, f"{SORTIE}_RANGE")
    with open(cfg_path, "w", encoding="utf-8", newline="\n") as f:
        f.write(json.dumps(cfg, indent=2) + "\n")
    print(f"wrote {out_miz}")
    print(f"wrote {cfg_path}")
    print(f"  {len(SECTORS)} sectors, {len(ZONES)} zones, {len(DECK)} deck client groups, carriers CVN-72 (unit {cvn_uid}) and LHA-1 (unit {lha_uid})")
    print(f"  blue home fields: {', '.join(BLUE_FIELDS.values())}")
    print(f"  red home fields:  {', '.join(RED_FIELDS.values())}")


if __name__ == "__main__":
    main()
