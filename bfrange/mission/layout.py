# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""The range's layout: the whole Caucasus theatre divided into SECTORS.

Every sector has one job (a bombing range, a fight area, a tanker track...)
and is drawn on the F10 map from the mission's own drawings, so a pilot can
see at a glance where to go for what. The trigger zones the engine spawns
targets at live inside their sector, and the range config is generated from
the same table, so the picture on the map and what the engine does cannot
drift apart.

Positions are DCS lat/lon (the terrain's own projection, see caucasus_proj.json),
checked against DCS's 1:1M raster chart of the theatre. Ground sectors sit on
open, low ground away from towns: the Samgori/Iori steppe east of Tbilisi for
blue, the Terek-Kuma steppe north of Mozdok and the Kuban plain for red.

Shapes:
    ("rect", (lat, lon), east_west_m, north_south_m)
    ("circle", (lat, lon), radius_m)
    ("track", (lat, lon), heading_deg, leg_m, width_m)    a tanker race-track
"""

NM = 1852.0

# Home airfields (DCS reference points).
VAZIANI = (41.629, 45.027)
MOZDOK = (43.791, 44.620)
KUTAISI = (42.176, 42.482)
NALCHIK = (43.510, 43.636)

# Discipline colours for the F10 drawings, 0xRRGGBB. Fill alpha is added by
# the builder; the legend on the map explains them.
COLOURS = {
    "air_to_ground": 0xFF9F1A,   # amber
    "tactical":      0xFFE14D,   # yellow
    "threat":        0xFF3B3B,   # red
    "gunnery":       0xC98A4B,   # tan
    "helo":          0x7CFC4A,   # lime
    "air_to_air":    0x33D6FF,   # cyan
    "bvr":           0x4D8DFF,   # blue
    "duel":          0xFF66C4,   # pink
    "aar":           0x3DFF88,   # green
    "carrier":       0xF2F2F2,   # white
    "anti_ship":     0xC266FF,   # violet
}

KIND_LABEL = {
    "air_to_ground": "AIR-TO-GROUND",
    "tactical": "TACTICAL / CAS",
    "threat": "THREAT / SEAD",
    "gunnery": "CA GUNNERY",
    "helo": "HELICOPTER",
    "air_to_air": "AIR-TO-AIR",
    "bvr": "BVR",
    "duel": "DUELS",
    "aar": "AIR REFUELLING",
    "carrier": "CARRIER OPS",
    "anti_ship": "ANTI-SHIP",
}

# id, name, kind, for ("blue" | "red" | "all"), shape, one line of what is in it
SECTORS = [
    # ---- blue, east Georgia: the Samgori / Iori steppe and the hills south-west of Tbilisi
    dict(id="r-1", name="R-1 SAMGORI", kind="air_to_ground", side="blue",
         shape=("rect", (41.603, 45.205), 11000, 7000),
         purpose="Bomb circle, strafe pit (run in from the east)"),
    dict(id="g-1", name="G-1 LILO", kind="gunnery", side="blue",
         shape=("rect", (41.668, 45.190), 5000, 3200),
         purpose="Combined Arms gunnery lane"),
    dict(id="r-2", name="R-2 IORI", kind="tactical", side="blue",
         shape=("rect", (41.600, 45.360), 12000, 9000),
         purpose="Tactical array (laser 1688), moving convoy, JDAM targets. JTAC Axeman 133.0 AM"),
    dict(id="r-3", name="R-3 TETRI TSKARO", kind="threat", side="blue",
         shape=("rect", (41.490, 44.400), 14000, 9000),
         purpose="SA-8 site, radar on / weapons hold. Instructors can spawn more SAMs"),
    dict(id="h-1", name="H-1 VAZIANI", kind="helo", side="blue",
         shape=("rect", (41.640, 45.080), 7500, 5200),
         purpose="Precision pad, confined LZ, pinnacle, sling-load and troop courses"),
    dict(id="h-2", name="H-2 TSKALTUBO", kind="helo", side="blue",
         shape=("rect", (42.305, 42.550), 13000, 21000),
         purpose="Mountain flying: pads, confined LZ, pinnacle, sling and troop courses near Kutaisi"),
    dict(id="moa-kakheti", name="MOA KAKHETI", kind="air_to_air", side="blue",
         shape=("circle", (41.930, 45.180), 11 * NM),
         purpose="BFM over the mountains. Floor 8,000 ft"),
    dict(id="ar-4", name="AR-4 TEXACO 2", kind="aar", side="blue",
         shape=("track", (41.950, 43.550), 90, 30 * NM, 8 * NM),
         purpose="KC-135 boom, FL240, TACAN 54Y, 254.0"),

    # ---- red, the Terek-Kuma steppe north of Mozdok, the Kuban plain, the Nalchik foothills
    dict(id="r-11", name="R-11 STEPNOYE", kind="air_to_ground", side="red",
         shape=("rect", (44.430, 44.460), 11000, 7000),
         purpose="Bomb circle, strafe pit (run in from the east)"),
    dict(id="g-11", name="G-11 STARODUB", kind="gunnery", side="red",
         shape=("rect", (44.475, 44.180), 5000, 3200),
         purpose="Combined Arms gunnery lane"),
    dict(id="r-12", name="R-12 KARST", kind="tactical", side="red",
         shape=("rect", (44.385, 44.280), 12000, 8000),
         purpose="Tactical array (laser 1511), moving convoy, JDAM targets. JTAC Topor 124.5 AM"),
    dict(id="r-13", name="R-13 ACHIKULAK", kind="threat", side="red",
         shape=("rect", (44.555, 44.430), 14000, 10000),
         purpose="SA-8 site, radar on / weapons hold. Instructors can spawn more SAMs"),
    dict(id="r-14", name="R-14 KUBAN", kind="air_to_ground", side="red",
         shape=("rect", (44.960, 40.225), 10000, 6000),
         purpose="Bomb circle, strafe pit, tactical array (laser 1512) for Maykop and Krymsk"),
    dict(id="h-11", name="H-11 MOZDOK", kind="helo", side="red",
         shape=("rect", (43.800, 44.673), 7500, 5200),
         purpose="Precision pad, confined LZ, pinnacle, sling-load and troop courses"),
    dict(id="h-12", name="H-12 NALCHIK", kind="helo", side="red",
         shape=("rect", (43.492, 43.685), 8500, 12000),
         purpose="Mountain flying: pads, confined LZ, pinnacle, sling and troop courses"),
    dict(id="moa-nogai", name="MOA NOGAI", kind="air_to_air", side="red",
         shape=("circle", (44.250, 45.300), 15 * NM),
         purpose="BFM over the steppe. Floor 5,000 ft"),
    dict(id="ar-5", name="AR-5 ILYUSHA", kind="aar", side="red",
         shape=("track", (44.650, 43.300), 90, 30 * NM, 8 * NM),
         purpose="IL-78M basket, FL230, TACAN 59Y, 124.0"),
    dict(id="ar-6", name="AR-6 ILYUSHA WEST", kind="aar", side="red",
         shape=("track", (44.300, 37.300), 90, 30 * NM, 8 * NM),
         purpose="IL-78M basket, FL220, TACAN 58Y, 123.0"),

    # ---- the Black Sea, shared
    dict(id="oparea", name="CV OPAREA", kind="carrier", side="all",
         shape=("circle", (41.700, 41.000), 20 * NM),
         purpose="CVN-72 TACAN 72X ICLS 11 Link-4 336.0 | LHA-1 TACAN 71X | recovery tanker A-6E 63Y 261.0"),
    dict(id="as-1", name="AS-1 SHIPPING", kind="anti_ship", side="all",
         shape=("rect", (42.330, 40.480), 70000, 45000),
         purpose="Undefended merchant ships steaming west. Spawn warships from the site or F10"),
    dict(id="w-1", name="W-1 BFM NORTH", kind="air_to_air", side="all",
         shape=("circle", (42.300, 39.450), 15 * NM),
         purpose="BFM set-ups and duels. Floor 5,000 ft"),
    dict(id="w-2", name="W-2 BFM SOUTH", kind="air_to_air", side="all",
         shape=("circle", (41.750, 39.450), 15 * NM),
         purpose="BFM set-ups and duels. Floor 5,000 ft"),
    dict(id="w-3", name="W-3 BVR", kind="bvr", side="all",
         shape=("rect", (43.500, 38.050), 120000, 100000),
         purpose="BVR presentations, Fox 1 / Fox 3 missile defence"),
    dict(id="w-4", name="W-4 DUEL", kind="duel", side="all",
         shape=("circle", (43.200, 39.450), 15 * NM),
         purpose="Blue vs red: meet here. Missile trainer on, guns are real"),
    dict(id="ar-1", name="AR-1 TEXACO", kind="aar", side="blue",
         shape=("track", (42.750, 39.100), 90, 30 * NM, 8 * NM),
         purpose="KC-135 boom, FL250, TACAN 51Y, 251.0"),
    dict(id="ar-2", name="AR-2 ARCO", kind="aar", side="blue",
         shape=("track", (42.250, 38.200), 90, 25 * NM, 8 * NM),
         purpose="KC-135MPRS basket, FL210, TACAN 52Y, 252.0"),
    dict(id="ar-3", name="AR-3 SHELL", kind="aar", side="blue",
         shape=("track", (41.600, 39.950), 90, 20 * NM, 6 * NM),
         purpose="KC-130 basket (helos too), 14,000 ft, TACAN 53Y, 253.0"),
]

# Where the legend sits (open sea, west).
LEGEND_AT = (42.85, 37.05)

# Sectors this small get a two-line tag on the map; their details go in their
# side's summary box, placed beside the cluster. DCS draws text at a fixed
# screen size, so full labels on a tight cluster pile on top of each other.
SMALL_KINDS = ("air_to_ground", "tactical", "threat", "gunnery", "helo")

SUMMARIES = {
    "blue": dict(at=(41.395, 44.620), title="BLUE RANGES - GEORGIA", ids=["r-1", "r-2", "r-3", "g-1", "h-1", "h-2"]),
    "red": dict(at=(44.262, 44.050), title="RED RANGES - NORTH OF MOZDOK AND THE KUBAN",
                ids=["r-11", "r-12", "r-13", "g-11", "r-14", "h-11", "h-12"]),
}


# ------------------------------------------------------------------ zones
#
# The trigger zones the config places things at, named <SECTOR>-<WHAT> so the
# Mission Editor list reads like the map. Positions are offsets in metres
# (north, east) from a sector's centre, or from an airfield for helicopter
# courses that start on the field.

import math  # noqa: E402

_BY_ID = {s["id"]: s for s in SECTORS}


def centre(sector_id):
    return _BY_ID[sector_id]["shape"][1]


def at(base, north_m=0.0, east_m=0.0):
    """(lat, lon) `north_m`/`east_m` from `base` (a sector id or a lat/lon)."""
    lat, lon = centre(base) if isinstance(base, str) else base
    return (lat + north_m / 111_132.0,
            lon + east_m / (111_320.0 * math.cos(math.radians(lat))))


def _helo_course(p, base, pinnacle):
    """The standard helicopter course around `base`: pads and pickups next to
    each other, drop zones and the LZ further out, the pinnacle on high ground."""
    return [
        (f"{p}-PAD", at(base, 0, 2500), 30),
        (f"{p}-CONFINED", at(base, 1200, 3500), 40),
        (f"{p}-PINNACLE", pinnacle, 30),
        (f"{p}-SLING1-PICKUP", at(base, -600, 1500), 50),
        (f"{p}-SLING1-DZ", at(base, -800, 6500), 50),
        (f"{p}-SLING2-PICKUP", at(base, -600, 1900), 50),
        (f"{p}-SLING2-DZ", at(base, 1500, 7000), 60),
        (f"{p}-TROOPS-PICKUP", at(base, -400, 1700), 60),
        (f"{p}-TROOPS-LZ", at(base, 3000, 7000), 80),
    ]


# The Samgori and Vaziani spots are the ones the first range mission shipped
# with (bomb circle at 41.600N 45.200E).
_SAMGORI = (41.600, 45.200)

ZONES = [
    # R-1 SAMGORI: bomb circle west, strafe pit east (run-in from the east
    # keeps strafers off the Vaziani helicopter courses)
    ("R1-BOMB", at("r-1", -300, -2100), 150),
    ("R1-STRAFE", at("r-1", -300, 1900), 150),
    # G-1 LILO
    ("G1-LANE", at("g-1", -800, 0), 150),
    # R-2 IORI
    ("R2-ARRAY", at("r-2", 1600, -1700), 200),
    ("R2-CONVOY", at("r-2", -2500, -3500), 150),
    ("R2-CONVOY-END", at("r-2", -2500, 3300), 150),
    ("R2-JDAM", at("r-2", 2200, 2900), 250),
    # R-3 TETRI TSKARO
    ("R3-SAM", centre("r-3"), 300),
    # H-1 VAZIANI (the first mission's helicopter courses)
    *_helo_course("H1", VAZIANI, at(VAZIANI, 2500, 5000)),
    # H-2 TSKALTUBO: courses on the fields north of the railway, pinnacle up
    # in the hills north of Kutaisi
    *_helo_course("H2", at(KUTAISI, 6000, 2000), (42.390, 42.560)),
    # R-11 STEPNOYE: bomb circle west, strafe pit east
    ("R11-BOMB", at("r-11", 0, -1500), 150),
    ("R11-STRAFE", at("r-11", 0, 1500), 150),
    # G-11 STARODUB
    ("G11-LANE", at("g-11", -800, 0), 150),
    # R-12 KARST
    ("R12-ARRAY", at("r-12", 1600, -1700), 200),
    ("R12-CONVOY", at("r-12", -2500, -3500), 150),
    ("R12-CONVOY-END", at("r-12", -2500, 3300), 150),
    ("R12-JDAM", at("r-12", 2200, 2900), 250),
    # R-13 ACHIKULAK
    # north-east corner: the Gor'kaya balka stream crosses the middle
    ("R13-SAM", at("r-13", 2200, 2800), 300),
    # R-14 KUBAN: west of the small river through Goncharov
    ("R14-BOMB", at("r-14", -900, -2600), 150),
    ("R14-STRAFE", at("r-14", -900, 1500), 150),
    ("R14-ARRAY", at("r-14", 1500, 0), 200),
    # H-11 MOZDOK (the first mission's red helicopter courses)
    *_helo_course("H11", MOZDOK, at(MOZDOK, 2500, 5000)),
    # H-12 NALCHIK: courses on the farmland east of the field, pinnacle on
    # the foothills south-east of the city
    *_helo_course("H12", NALCHIK, (43.448, 43.675)),
]
