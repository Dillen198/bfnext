# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""Build the Vector Strike training range mission (Mariana Islands) and its config.

    python build_range_miz.py [--donor donor/marianas_sua.miz] [--out DIR]

Writes, into DIR (default: E:/Saved Games/DCS/Missions/Vector/range):

    VS_Range_Marianas.miz   the mission: sortie VSRANGE, no campaign content
    VSRANGE_RANGE           the matching bfrange config (JSON)

The mission is DERIVED from a MarianaIslands donor rather than written from
scratch, so everything a script can't easily produce correctly -- theatre,
weather, options, the dictionary, every airfield's warehouse, the briefing
pictures (the NAVFAC range complex chart, the Andersen plates) -- is the
Mission Editor's own output. The donor (donor/marianas_sua.miz) is the
Marianas SUA mission: no units, the real airspace as drawings. From it we
keep the theatre, options, warehouses and briefing pictures; its drawings
are replaced by the range's own.

and we add:

  * CVN-72 (Supercarrier) and LHA-1 (Tarawa) in the carrier op area,
  * the CJTF Blue / CJTF Red countries if the donor has none,
  * deck client slots: F/A-18C x4, F-14B x2, F-14A x2 on CVN-72; AV-8B x2
    and UH-1H x2 on LHA-1 (dynamic spawn can't use a Supercarrier deck),
  * Combined Arms slots (tactical commander, JTAC/operator, observer),
  * the trigger zones the range config places things in,
  * the bfrange loader as the mission's init script.

The layout lives in layout.py: the real Mariana Islands Range Complex
airspace, and the range's SECTORS placed inside it with one job each. From
that one table this script writes the trigger zones, the range config
(including its `sectors`) and the Mission Editor drawings -- the real
airspace in grey, each sector outlined in its discipline's colour with a
label, plus a legend -- so the F10 map and the engine always agree. The
islands are small: the engine moves any target that lands in water to dry
ground, but open the mission in the Mission Editor once and eyeball the zones.
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
import tmproj  # noqa: E402

DEFAULT_DONOR = os.path.join(HERE, "donor", "marianas_sua.miz")
DEFAULT_OUT = r"E:/Saved Games/DCS/Missions/Vector/range"
SORTIE = "VSRANGE"
MIZ_NAME = "VS_Range_Marianas.miz"

# ------------------------------------------------------------------ layout
#
# The theatre is divided into SECTORS, one job each, in layout.py. The zones,
# the range config and the F10 drawings are all generated from that table.

import layout  # noqa: E402
import markers  # noqa: E402
import training  # noqa: E402
from layout import SECTORS, NM  # noqa: E402,F401

# ------------------------------------------------------------------ geometry

_PROJ = tmproj.Projection(layout.THEATRE)


def ll2xz(lat, lon):
    """lat/lon -> DCS x (north), z (east): the terrain's own transverse Mercator."""
    return _PROJ.ll2xz(lat, lon)


def xz2ll(x, z):
    return _PROJ.xz2ll(x, z)


ZONES = layout.ZONES

CVN_POS = layout.centre("oparea")
LHA_POS = layout.at("oparea", -3000, -5000)

# DCS airdrome ids on MarianaIslands. Blue is the only side with slots: the
# range launches from Guam the way a real exercise launches from Andersen.
# Rota, Saipan and Tinian sit under the threat, hot zone and jamming sectors.
BLUE_FIELDS = {6: "Andersen AFB", 4: "Antonio B. Won Pat Intl"}
RED_FIELDS = {}

# deck slots: (carrier, category, type, count, group name, callsign id, callsign name, fuel kg, task)
DECK = [
    ("CVN-72", "plane", "FA-18C_hornet", 2, "CVN Hornet 1", 1, "Enfield", 4900, "CAP"),
    ("CVN-72", "plane", "FA-18C_hornet", 2, "CVN Hornet 2", 2, "Springfield", 4900, "CAP"),
    ("CVN-72", "plane", "F-14B", 2, "CVN Tomcat B", 3, "Uzi", 7348, "CAP"),
    ("CVN-72", "plane", "F-14A-135-GR", 2, "CVN Tomcat A", 4, "Colt", 7348, "CAP"),
    ("LHA-1", "plane", "AV8BNA", 2, "LHA Harrier", 5, "Dodge", 3519, "CAS"),
    ("LHA-1", "helicopter", "UH-1H", 2, "LHA Huey", 6, "Ford", 631, "Transport"),
]

BRIEFING = """VECTOR STRIKE TRAINING RANGE - MARIANA ISLANDS

A practice server. No campaign, no lives, no points: every slot is open and
everything you fly is graded. Results, debriefs, the greenie board and the
spawner are at https://range.vectorstrike.org

THE MARIANA ISLANDS RANGE COMPLEX
The thin outlines on the F10 map are the real special use airspace around
Guam and the Northern Marianas (the briefing picture is the NAVFAC chart):
the ATCAAs over the warning areas, W-517, and R-7201 round Farallon de
Medinilla. Every block has a job, the one it has in life; the filled areas
inside are the ranges. F10 > Range > Sectors lists them nearest first with
bearing and range, and you are told what a sector is for when you fly in.
The ranges are marked on the ground like real ones: tyre rings round the
bomb circle and the arrays, a tyre foul line on the strafe pit, flags on the
coordinate targets and the gunnery lane, tyre pads and cones for helicopters.
  amber  air-to-ground     yellow  tactical / CAS    red    threat / SEAD
  tan    CA gunnery        lime    helicopter        cyan   air-to-air
  blue   BVR               green   tanker tracks     white  carrier ops
  violet anti-ship         hot red  HOT ZONE         teal   EW / GPS jamming
  white line  low-level route    lime dashed  CSAR   white dashed  pattern

FLY IT LIKE THE REAL THING
  Launch from Andersen or Won Pat (dynamic spawn, hot start) or the decks.
  Work inside your airspace; R-7201 round Farallon de Medinilla is the
  only live impact area at sea. Tankers sit on the published tracks: fly
  the entry line from the ARIP, join at the ARCP, leave on the exit line.

AIR-TO-AIR
  W-517              BFM / ACM, live missile shots (SW of Guam)
  ATCAA 2 (W-12)     duels, round W-517
  ATCAA 5            BFM west of Guam
  ATCAA 1 (W-11)     BVR presentations, Fox 1 / Fox 3 defence
  ATCAA 3C (W-13C)   intercepts east of the chain
  ATCAA 3A (W-13A)   large-force strikes into R-7201 against red air
  ATCAA 6            over the islands: intercepts and DCA above FL150
AIR-TO-GROUND
  R-7201A            the 12 nm ring round FDM: hold, then run in
  R-7201 FDM         bomb circle, strafe pit (run in 209 from the NNE), JDAM
  TN-1 NORTH FIELD   Tinian: tactical array laser 1688, convoy, JDAM,
                     JTAC Axeman 133.0
  TN-2 TINIAN MLA    tiered targets: easy / medium (AAA) / hard (SHORAD)
  EW-1 TINIAN SOUTH  GPS jamming over coordinate targets
THREAT
  R-3 AGUIJAN        SA-8 emitter (radar on, weapons hold)
  S-1 ROTA           LIVE SEAD/IADS: EWR, SA-2/3/6/11/10, Pantsir - weapons free
  HZ-1 SAIPAN        HOT ZONE: red CAP + SHORAD, AWACS Overlord 1 on 251.5
GUAM
  H-1 ANDERSEN       helicopter courses, Andersen South LZ, Mt Santa Rosa
  G-1 NW FIELD       Combined Arms gunnery lane
  PT-1 ANDERSEN      overhead breaks; every landing everywhere is graded
  CS-1 SOUTH GUAM    CSAR, beacon 350 kHz (Helicopter > CSAR)
  LL-1 GUAM COAST    low-level route Ritidian to Orote, 8 gates, below 500 ft
AT SEA
  CV OPAREA          CVN-72 TACAN 72X ICLS 11 Link-4 336.0, LHA-1 71X,
                     recovery tanker A-6E 63Y 261.0. Every pass graded.
  FD-1 APRA          frigate decks: FFG-7 41X, DDG 42X
  AS-1 SINKEX        undefended hulks in W-517
  ATCAA 3B (W-13B)   maritime strike; AS-2 escorted convoy, weapons free
TANKERS
  AR RIO     KC-135 boom        51Y 251.0  FL250     300 KIAS
  AR MIRAGE  KC-135MPRS basket  52Y 252.0  FL210     270 KIAS, next to FDM
  AR LUXOR   KC-135 boom        54Y 254.0  FL240     300 KIAS
  AR SHELL   KC-130 basket      53Y 253.0  14,000 ft 220 KIAS, west of Guam
  Recovery tanker A-6E over the carrier: 63Y 261.0, 6,000 ft, 250 KIAS

The MISSILE TRAINER removes any missile that would kill you and tells you
so - SAMs included, so the SEAD range, hot zone and escorted convoy are
live fire you survive. Guns and AAA are real. Chat: -range trainer on|off
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
    if kind == "poly":
        return [ll2xz(*p) for p in shape[1]]
    if kind == "route":
        _, gates, width = shape
        pts = [ll2xz(*g) for g in gates]
        r = width / 2
        left, right = [], []
        for i, (x, z) in enumerate(pts):
            # mitred normal: the average of the neighbouring legs' directions
            dirs = []
            for a, b in ((i - 1, i), (i, i + 1)):
                if 0 <= a and b < len(pts):
                    dx, dz = pts[b][0] - pts[a][0], pts[b][1] - pts[a][1]
                    n = math.hypot(dx, dz) or 1
                    dirs.append((dx / n, dz / n))
            fx = sum(d[0] for d in dirs) / len(dirs)
            fz = sum(d[1] for d in dirs) / len(dirs)
            n = math.hypot(fx, fz) or 1
            fx, fz = fx / n, fz / n
            left.append((x - fz * r, z + fx * r))
            right.append((x + fz * r, z - fx * r))
        return left + right[::-1]
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
    if shape[0] == "route":
        x, z = ll2xz(*shape[1][0])
        return x + 2500, z + 1500
    if shape[0] == "poly":
        # the northernmost corner: airspace has no straight north-west edge
        return max(ring)
    return max(p[0] for p in ring), min(p[1] for p in ring)


def inscribed_circle(shape):
    """The largest circle inside a sector, (centre lat/lon, radius m): where a
    polygon's fight-area circle goes. Rects and circles are their own."""
    if shape[0] == "circle":
        return shape[1], shape[2]
    ring = sector_ring(shape)

    def inside(x, z):
        hit = False
        for (x1, z1), (x2, z2) in zip(ring, ring[1:] + ring[:1]):
            if (z1 > z) != (z2 > z) and x < x1 + (z - z1) * (x2 - x1) / (z2 - z1):
                hit = not hit
        return hit

    def edge_dist(x, z):
        best = float("inf")
        for (x1, z1), (x2, z2) in zip(ring, ring[1:] + ring[:1]):
            dx, dz = x2 - x1, z2 - z1
            t = max(0.0, min(1.0, ((x - x1) * dx + (z - z1) * dz) / (dx * dx + dz * dz)))
            best = min(best, math.hypot(x - x1 - t * dx, z - z1 - t * dz))
        return best

    xs, zs = [p[0] for p in ring], [p[1] for p in ring]
    best = (0.0, None)
    for i in range(41):
        for j in range(41):
            x = min(xs) + (max(xs) - min(xs)) * i / 40
            z = min(zs) + (max(zs) - min(zs)) * j / 40
            if inside(x, z):
                d = edge_dist(x, z)
                if d > best[0]:
                    best = (d, (x, z))
    return xz2ll(*best[1]), best[0]


# ------------------------------------------------------------------ config

def cfg_loc(zone):
    return {"zone": zone}


def cfg_ll(ll):
    return {"lat": round(ll[0], 5), "lon": round(ll[1], 5)}


def conventional(p, sid, name, side, country, strafe_hdg=270, run_in="the east"):
    """A bomb circle and a strafe pit, flown in on `strafe_hdg` from `run_in`."""
    return [
        {"id": f"{sid}-bomb", "name": f"{name} - Bomb Circle", "kind": "bomb_circle",
         "loc": cfg_loc(f"{p}-BOMB"), "rings_m": [25, 50, 100, 150], "side": side, "country": country,
         "targets": [{"typ": "Container red 1", "category": "static", "name": f"Circle {name.split()[0]}"}],
         "note": "Unguided and guided bombs, rockets. Any run-in heading."},
        {"id": f"{sid}-strafe", "name": f"{name} - Strafe Pit", "kind": "strafe_pit",
         "loc": cfg_loc(f"{p}-STRAFE"), "heading_deg": strafe_hdg, "side": side, "country": country,
         "strafe": {"box_length_m": 3000, "box_width_m": 300, "foul_line_m": 610, "max_alt_agl_m": 914},
         "targets": [{"typ": "Container white", "category": "static", "y": -15},
                     {"typ": "Container white", "category": "static", "y": 15}],
         "note": f"Foul line 2000 ft. Run in from {run_in}, heading {strafe_hdg:03d}."},
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


def coord_targets(p, sid, name, side, country):
    return {"id": f"{sid}-jdam", "name": f"{name} - Coordinate Targets", "kind": "coord_target",
            "loc": cfg_loc(f"{p}-JDAM"), "side": side, "country": country, "respawn_s": 900,
            "targets": [{"typ": "Hangar A", "category": "static", "name": "Hangar"},
                        {"typ": "Garage A", "category": "static", "x": 120, "y": 60, "name": "Garage"},
                        {"typ": "FARP CP Blindage", "category": "static", "x": -150, "y": 80, "name": "Command post"}],
            "note": "JDAM / JSOW / coordinate weapons. Coordinates: F10 > Range > Air-to-Ground > Stations."}


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
        coord_targets(p, sid, name, side, country),
    ]


def threat(p, sid, name, side, country):
    return [{"id": f"{sid}-sam", "name": f"{name} - SA-8 Threat", "kind": "sam_site",
             "loc": cfg_loc(f"{p}-SAM"), "side": side, "country": country,
             "targets": [{"typ": "Osa 9A33 ln", "category": "vehicle"}], "weapons_free": False, "locked": True,
             "note": "Radar on, weapons hold. Instructors can go weapons free; the missile trainer protects you."}]


def gunnery(p, sid, name, side, country, heading=0):
    return [{"id": f"{sid}-lane", "name": f"{name} - CA Gunnery Lane", "kind": "gunnery_lane",
             "loc": cfg_loc(f"{p}-LANE"), "heading_deg": heading, "side": side, "country": country, "respawn_s": 120,
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


# tanker per AR sector: id, callsign, number, type, alt ft, KIAS, MHz, TACAN channel, morse, side, country.
# Speeds are indicated, as flown on the boom or the basket: KC-135 boom 300
# for fighters, the MPRS basket 270 for the Navy jets, the KC-130 220 for
# the Harrier and the slower jets. The engine turns them into true airspeed
# at the track's altitude for DCS (300 KIAS at FL250 is about 430 kts true).
TANKERS = {
    "ar-rio": ("texaco", "Texaco", 1, "KC-135", 25000, 300, 251.0, 51, "TEX", "blue", "CJTF Blue"),
    "ar-mirage": ("arco", "Arco", 1, "KC135MPRS", 21000, 270, 252.0, 52, "ARC", "blue", "CJTF Blue"),
    "ar-shell": ("shell", "Shell", 1, "KC130", 14000, 220, 253.0, 53, "SHL", "blue", "CJTF Blue"),
    "ar-luxor": ("texaco2", "Texaco", 2, "KC-135", 24000, 300, 254.0, 54, "TX2", "blue", "CJTF Blue"),
}

ARENA_FLOOR_FT = {"atcaa-6": 15000}


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
        pts = [xz2ll(x, z) for x, z in sector_ring(shape)]
        geo = {"polygon": [{"lat": round(a, 5), "lon": round(o, 5)} for a, o in pts]}
    return {"id": s["id"], "name": s["name"], "kind": s["kind"], "side": s["side"],
            "purpose": s["purpose"], "shape": geo}


def build_config():
    red = ("red", "CJTF Red")
    stations = (
        # blue only: every target is red kit
        conventional("R7201", "r7201", "R-7201 FDM", *red, strafe_hdg=layout.FDM_STRAFE_HDG, run_in="the NNE along the island")
        + [coord_targets("R7201", "r7201", "R-7201 FDM", *red)]
        + tactical("TN1", "tn1", "TN-1 North Field", *red, 1688)
        + threat("R3", "r3", "R-3 Aguijan", *red)
        + gunnery("G1", "g1", "G-1 Northwest Field", *red, heading=60)
        + training.extra_stations()
        + [{"id": "as1-ships", "name": "AS-1 SINKEX - Hulks", "kind": "ship_target",
            "loc": cfg_ll(layout.at("as-1", -3300, 21600)), "side": "red", "country": "CJTF Red", "respawn_s": 900,
            "targets": [{"typ": "Dry-cargo ship-1", "category": "ship"},
                        {"typ": "ELNYA", "category": "ship", "y": 600}],
            "route": {"points": [cfg_ll(layout.at("as-1", -3300, -23000))], "speed_kts": 12},
            "note": "Undefended SINKEX hulks for anti-ship missiles and bombs."}]
    )
    helo = helo_cfg("H1", "h1", "H-1 Andersen")
    tankers = []
    for s in SECTORS:
        if s["id"] in TANKERS:
            i, cs, n, typ, alt, kias, mhz, ch, morse, side, country = TANKERS[s["id"]]
            _, start, hdg, leg, _ = s["shape"]
            tankers.append({"id": i, "callsign": cs, "callsign_number": n, "typ": typ, "side": side, "country": country,
                            "loc": cfg_ll(start), "heading_deg": hdg, "leg_nm": round(leg / NM, 1), "alt_ft": alt, "speed_kias": kias,
                            "freq_mhz": mhz, "tacan": {"channel": ch, "band": "Y", "morse": morse}})
    arenas = []
    for s in SECTORS:
        if s["kind"] not in ("air_to_air", "bvr", "duel"):
            continue
        c, radius = inscribed_circle(s["shape"])
        arenas.append({"id": s["id"], "name": s["name"], "loc": cfg_ll(c), "radius_nm": round(radius / NM, 1),
                       "duels": True, "floor_ft": ARENA_FLOOR_FT.get(s["id"], 5000)})
    return {
        "netidx_base": "/local/fowl/range",
        "name": "Vector Strike Range",
        "welcome": "Welcome to the Vector Strike Range, Mariana Islands. The thin outlines on the F10 map are the real range complex airspace and the filled areas inside are the ranges; every one has a job and F10 > Range > Sectors lists them. The missile trainer is ON. Debriefs and spawning: range.vectorstrike.org",
        "sectors": [sector_cfg(s) for s in SECTORS],
        "stations": stations,
        "tankers": tankers,
        "carriers": [
            {"id": "cvn72", "unit_name": "CVN-72", "name": "CVN-72 Abraham Lincoln", "kind": "nimitz",
             "tacan": {"channel": 72, "band": "X", "morse": "ABL"}, "icls_channel": 11, "acls": True,
             "link4_mhz": 336.0, "tower_mhz": 305.0, "wind_over_deck_kts": 27,
             "op_center": cfg_ll(CVN_POS), "op_radius_nm": 20,
             "recovery_tanker": {"id": "cvn72-tanker", "callsign": "Arco", "callsign_number": 3, "typ": "A6E",
                                 "loc": cfg_ll(CVN_POS), "heading_deg": 0, "leg_nm": 10, "alt_ft": 6000, "speed_kias": 250,
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
        # C-130J airdrops: the ground crew's cargo (F8 at Andersen or Won Pat),
        # out of the ramp over Orote's disused airfield, run in along its old
        # runway; anything air-landed at another field counts too
        "airdrop": {"reach_m": 1500, "zones": [
            {"id": "dz1-orote", "name": "Orote", "loc": cfg_ll((13.4380, 144.6350)), "perfect_m": 50, "run_in_deg": 70},
        ]},
        "jtacs": [
            {"id": "axeman", "callsign": "Axeman", "station": "tn1-array", "loc": cfg_loc("TN1-ARRAY"),
             "laser_code": 1688, "freq_mhz": 133.0, "typ": "MQ-9 Reaper", "friendlies_m": 600},
        ],
        **training.config_sections(),
        "spawn": {"max_active_per_player": 3, "max_ai_units": 60, "cooldown_s": 20, "despawn_after_s": 3600,
                  "despawn_on_leave": True, "instructor_only": ["sam_site", "naval_group"], "instructors": []},
        "in_game_results": True,
        "message_s": 20,
        "record_tracks": True,
        # the real weather and time, written into the mission before every
        # load (bfrange/README.md, "Live weather"): Andersen's METAR, Guam
        # time (UTC+10, no daylight saving). Put the CheckWX key in on the
        # server; without one the surface comes from open-meteo's model too
        "live_weather": {"lat": 13.584, "lon": 144.930, "metar_station": "PGUA", "checkwx_api_key": None,
                         "sync_time": True, "utc_offset_hours": 10},
    }


# ------------------------------------------------------------------ drawings

def _rgba(rgb, alpha):
    return "0x%06x%02x" % (rgb, alpha)


LAYER = {"blue": "Blue", "red": "Red", "all": "Common"}


def _textbox(name, layer, at, text, rgb, size=9, alpha=0x00):
    """A label: small coloured text with no border, and by default no
    background, so it never hides the sectors under it."""
    return {"visible": True, "layerName": layer, "primitiveType": "TextBox",
            "mapX": at[0], "mapY": at[1], "name": name, "text": text,
            "font": "DejaVuLGCSansCondensed.ttf", "fontSize": size, "borderThickness": 0,
            "colorString": _rgba(rgb, 0xff), "fillColorString": "0x101418%02x" % alpha, "angle": 0}


def ar_line_drawings():
    """Every published refuelling track's entry and exit lines, with the
    ARIP and ARCP marked."""
    out = []
    green = layout.COLOURS["aar"]
    for name, t in layout.AR_TRACKS.items():
        for leg in ("entry", "exit"):
            pts = [ll2xz(*p) for p in t[leg]]
            x0, z0 = pts[0]
            out.append({"visible": True, "layerName": "Common", "primitiveType": "Line", "lineMode": "segments",
                        "name": f"{name.upper()} {leg}", "mapX": x0, "mapY": z0, "closed": False,
                        "colorString": _rgba(green, 0xb0), "thickness": 2, "style": "dash" if leg == "exit" else "solid",
                        "points": [{"x": px - x0, "y": pz - z0} for px, pz in pts]})
        out.append(_textbox(f"{name.upper()} ARIP", "Common", ll2xz(*t["entry"][0]),
                            f"{name.upper()} ARIP", green, 8))
        out.append(_textbox(f"{name.upper()} ARCP", "Common", ll2xz(*t["entry"][-1]),
                            f"{name.upper()} ARCP", green, 8))
    return out


def sector_drawings():
    """Mission Editor drawings for every sector: its outline, filled lightly in
    its discipline's colour, and a label at its north-west corner. A side's
    sectors go on that side's layer (only that coalition sees them), shared
    ones on Common. Formats copied from Mission Editor output.

    Returns {layer name: [objects]}."""
    layers = {"Blue": [], "Red": [], "Common": []}
    layers["Common"] += ar_line_drawings()
    for s in SECTORS:
        rgb = layout.COLOURS[s["kind"]]
        shape = s["shape"]
        layer = LAYER[s["side"]]
        style = "dash" if s["kind"] in layout.DASHED_KINDS else "solid"
        common = {"visible": True, "layerName": layer, "primitiveType": "Polygon",
                  "colorString": _rgba(rgb, 0xe0), "fillColorString": _rgba(rgb, 0x12),
                  "thickness": 2 if s["kind"] == "aar" else 3, "style": style, "angle": 0}
        if shape[0] == "circle":
            _, c, r = shape
            x, z = ll2xz(*c)
            if s.get("airspace"):
                common = {**common, "fillColorString": _rgba(rgb, 0x00), "colorString": _rgba(rgb, 0xb0), "thickness": 2}
            layers[layer].append({**common, "polygonMode": "circle", "radius": r, "mapX": x, "mapY": z, "name": s["name"]})
        elif shape[0] == "route":
            pass  # drawn as a line with its gates by training.extra_drawings
        elif s.get("airspace"):
            # real airspace: a closed line in the sector's colour, no fill (the
            # big blocks overlap everything else)
            ring = sector_ring(shape)
            x0, z0 = ring[0]
            layers[layer].append({"visible": True, "layerName": layer, "primitiveType": "Line", "lineMode": "segments",
                                  "closed": True, "name": s["name"], "mapX": x0, "mapY": z0,
                                  "colorString": _rgba(rgb, 0xb0), "thickness": 2, "style": "solid",
                                  "points": [{"x": px - x0, "y": pz - z0} for px, pz in ring]})
        else:
            ring = sector_ring(shape)
            x0, z0 = ring[0]
            # DCS draws a free polygon from its anchor, so the anchor is the
            # first vertex and the first point is (0, 0), as the Mission Editor
            # writes it; distinct vertices only, DCS closes the ring itself
            layers[layer].append({**common, "polygonMode": "free", "mapX": x0, "mapY": z0, "name": s["name"],
                                  "points": [{"x": px - x0, "y": pz - z0} for px, pz in ring]})
        text = f"{s['name']}\n{s.get('tag') or layout.KIND_LABEL[s['kind']]}"
        layers[layer].append(_textbox(f"{s['name']} label", layer, sector_label_at(shape), text, rgb))
    for layer, objs in training.extra_drawings(ll2xz).items():
        layers[layer] += objs
    legend = ["VECTOR STRIKE RANGE - MARIANA ISLANDS",
              "Thin lines: the real range complex airspace",
              "(ATCAAs, W- warning areas, R-7201 FDM), each",
              "with a job; filled areas inside are the ranges.",
              "amber  air-to-ground     yellow  tactical / CAS",
              "red    threat / SEAD     tan     CA gunnery",
              "lime   helicopter        cyan    air-to-air",
              "blue   BVR               pink    duels",
              "green  tanker tracks     white   carrier ops",
              "violet anti-ship         hot red  HOT ZONE (PvE)",
              "teal   EW / GPS jamming  white line  low-level route",
              "lime dashed  CSAR        white dashed  landing pattern",
              "R- range   TN- Tinian   G- gunnery   H- helicopter",
              "AR tanker track: solid line = entry (ARIP to ARCP)",
              "AS- anti-ship  S- SEAD/IADS  HZ- hot zone",
              "EW- jamming  LL- low level  CS- CSAR",
              "PT- pattern  FD- frigate decks",
              "Red dashed rings: SAM threat rings",
              "F10 > Range > Sectors: bearing and range to each"]
    layers["Common"].append(_textbox("Range legend", "Common", ll2xz(*layout.LEGEND_AT), "\n".join(legend), 0xF2F2F2, 9, 0x60))
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
    w["clouds"].pop("preset", None)  # a preset overrides the density above

    # --- strip every group: the range brings its own
    for side in ("blue", "red", "neutrals"):
        for c in m["coalition"][side]["country"].values():
            for cat in ("plane", "helicopter", "vehicle", "ship", "static"):
                c.pop(cat, None)

    # --- the CJTF countries the range spawns as (the donor may have none)
    for side, cid, cname in (("blue", 80, "CJTF Blue"), ("red", 81, "CJTF Red")):
        countries = m["coalition"][side]["country"]
        if not any(c["name"] == cname for c in countries.values()):
            countries[len(countries) + 1] = lt({"id": cid, "name": cname})
    blue = next(c for c in m["coalition"]["blue"]["country"].values() if c["name"] == "CJTF Blue")
    blue["ship"] = T({"group": T()})
    ships = blue["ship"]["group"]

    # --- CVN-72 and LHA-1: in the op area, immortal, holding fire
    def carrier(typ, name, pos, freq):
        nonlocal gid, uid
        x, z = ll2xz(*pos)
        unit = {"type": typ, "unitId": uid, "skill": "Excellent", "x": x, "y": z, "name": name,
                "heading": 0, "modulation": 0, "frequency": freq}
        if typ.startswith("CVN_"):
            unit.update({"allowLso": True, "allowAirboss": True})
        ships[len(ships) + 1] = lt({
            "visible": False, "tasks": {}, "uncontrollable": False, "hidden": False,
            "groupId": gid, "name": f"{name} Group", "x": x, "y": z, "start_time": 0,
            "route": {"points": [{"alt": 0, "type": "Turning Point", "ETA": 0, "alt_type": "BARO", "x": x, "y": z,
                                  "formation_template": "", "speed_locked": True, "ETA_locked": True, "speed": 0,
                                  "action": "Turning Point",
                                  "task": combo([wrapped(1, {"id": "SetImmortal", "params": {"value": True}}),
                                                 wrapped(2, {"id": "Option", "params": {"value": 4, "name": 0}})])}]},
            "units": [unit],
        })
        gid += 1
        uid += 1
        return uid - 1, x, z

    cvn_uid, cx, cz = carrier("CVN_72", "CVN-72", CVN_POS, 127500000)
    lha_uid, lx, lz = carrier("LHA_Tarawa", "LHA-1", LHA_POS, 127600000)

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

    # --- the range markings on the ground: neutral, hidden on F10
    neutrals = m["coalition"]["neutrals"]["country"]
    un = next((c for c in neutrals.values() if c["name"] == markers.COUNTRY), None)
    if un is None:
        un = lt({"id": markers.COUNTRY_ID, "name": markers.COUNTRY})
        neutrals[len(neutrals) + 1] = un
    un["static"] = T({"group": T()})
    statics = un["static"]["group"]
    for name, typ, ll in markers.all_markers():
        x, z = ll2xz(*ll)
        statics[len(statics) + 1] = lt({
            "heading": 0, "groupId": gid, "hidden": True, "dead": False, "x": x, "y": z, "name": name,
            "route": {"points": [{"alt": 0, "type": "", "name": "", "y": z, "speed": 0, "x": x,
                                  "formation_template": "", "action": ""}]},
            "units": [{"category": "Fortifications", "type": typ, "unitId": uid, "x": x, "y": z,
                       "name": name, "heading": 0}],
        })
        gid += 1
        uid += 1

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
    dictionary[m["descriptionBlueTask"]] = "Blue: home fields Andersen AFB and Won Pat on Guam (dynamic spawn, hot start, every airframe); CVN-72 and LHA-1 deck slots. Bomb R-7201 at Farallon de Medinilla, work Tinian's lease area (TN-1, TN-2, EW-1), take down the IADS on Rota (S-1), fight in W-517 and ATCAA 1, tank on RIO, MIRAGE, LUXOR or SHELL."
    dictionary[m["descriptionRedTask"]] = "No red slots: the opposition is AI."
    dictionary[m["descriptionNeutralsTask"]] = ""
    # open on the whole theatre
    mx, mz = ll2xz(14.30, 145.20)
    m["map"] = T({"centerX": mx, "centerY": mz, "zoom": 1400000})
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
    # the decks: an airfield's warehouse, made a blue ship's (the Mission
    # Editor writes a ship warehouse in the same shape, lower-case coalition)
    template = next(iter(wh["airports"].values()))
    keep = T()
    for unit_id in (cvn_uid, lha_uid):
        e = copy.deepcopy(template)
        e["coalition"] = "blue"
        e["dynamicSpawn"] = True
        e["allowHotStart"] = True
        e["dynamicCargo"] = False
        keep[unit_id] = e
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
    if "initScriptFile" not in m:
        m["maxDictId"] = m.get("maxDictId", 0) + 1
        m["initScriptFile"] = f"ResKey_initScript_{m['maxDictId']}"
    init_key = m["initScriptFile"]
    old_init = res.get(init_key)
    res[init_key] = "bfrange_mizinit.lua"
    if old_init:
        files.pop("l10n/DEFAULT/" + old_init, None)
    files["l10n/DEFAULT/bfrange_mizinit.lua"] = LOADER.encode("utf-8")
    files[training.BEACON_FILE] = training.beacon_wav()

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
    print(f"  home fields: {', '.join(BLUE_FIELDS.values())}")


if __name__ == "__main__":
    main()
