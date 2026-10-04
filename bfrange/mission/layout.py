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
    ("route", [(lat, lon), ...], width_m)                  a low-level route's corridor
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
    "hot_zone":      0xFF1744,   # hot red
    "ew":            0x00C8B4,   # teal
    "low_level":     0xFFFFFF,   # white (a route line, not an area)
    "csar":          0x7CFC4A,   # helicopter lime, dashed
    "pattern":       0xF2F2F2,   # white, dashed
    "ship_deck":     0xF2F2F2,   # white, dashed
}

# Drawn with a dashed outline rather than a solid one.
DASHED_KINDS = ("aar", "csar", "pattern", "ship_deck", "hot_zone")

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
    "hot_zone": "HOT ZONE",
    "ew": "EW / GPS JAMMING",
    "low_level": "LOW-LEVEL ROUTE",
    "csar": "CSAR",
    "pattern": "LANDING PATTERN",
    "ship_deck": "SHIP DECKS",
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

# Where the legend sits: the empty south-west corner of the Black Sea, clear
# of every sea sector whichever way DCS grows the box from its anchor.
LEGEND_AT = (41.90, 35.85)

# Sectors that get a two-line tag (name and kind) instead of a full label.
# DCS draws text at a fixed screen size, so at theatre zoom full labels pile
# on top of each other; what each sector is for is on F10 > Range > Sectors
# and is announced when you fly in. Tanker tracks and the carrier keep their
# full labels: the frequencies and TACANs on them are what you need in flight.
SMALL_KINDS = ("air_to_ground", "tactical", "threat", "gunnery", "helo", "ew", "csar", "pattern", "ship_deck",
               "low_level", "hot_zone", "anti_ship", "air_to_air", "bvr", "duel")


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


# ------------------------------------------------------------------ training sectors
#
# The second wave (Sept 29 2026): live SEAD/IADS ranges, tiered air-to-ground,
# EW/GPS jamming, PvE hot zones with an AWACS, low-level routes, landing
# pattern fields, CSAR, frigate decks and escorted shipping. Placed on DCS's
# 1:1M chart like the first wave; none overlaps another sector. What goes in
# each is in the tables below; training.py turns them into config and drawings.

SENAKI_ARP = (42.24085, 42.04802)    # RWY 09/27
MINVODY_ARP = (44.22785, 43.08119)   # RWY 12/30

LL1_GATES = [("LL1-G1", (42.06, 42.42), "ALPHA"), ("LL1-G2", (42.12, 42.20), "BRAVO"),
             ("LL1-G3", (42.12, 41.93), "CHARLIE"), ("LL1-G4", (42.28, 41.83), "DELTA"),
             ("LL1-G5", (42.42, 41.86), "ECHO"), ("LL1-G6", (42.50, 42.05), "FOXTROT"),
             ("LL1-G7", (42.52, 42.25), "GOLF"), ("LL1-G8", (42.45, 42.40), "HOTEL")]
LL11_GATES = [("LL11-G1", (44.86, 38.08), "ALPHA"), ("LL11-G2", (44.81, 38.32), "BRAVO"),
              ("LL11-G3", (44.79, 38.56), "CHARLIE"), ("LL11-G4", (44.73, 38.80), "DELTA"),
              ("LL11-G5", (44.72, 39.02), "ECHO"), ("LL11-G6", (44.64, 39.30), "FOXTROT"),
              ("LL11-G7", (44.57, 39.54), "GOLF"), ("LL11-G8", (44.61, 39.82), "HOTEL")]

SECTORS += [
    # ---- blue
    dict(id="s-1", name="S-1 AKHALKALAKI", kind="threat", side="blue",
         shape=("rect", (41.43, 43.40), 60000, 45000),
         purpose="LIVE SEAD/IADS: EWR, SA-2, SA-3, SA-6, SA-11, SA-10, Pantsir. Weapons free, networked, trainer protects you"),
    dict(id="r-4", name="R-4 GAREJI", kind="air_to_ground", side="blue",
         shape=("rect", (41.505, 45.33), 15000, 8000),
         purpose="Tiered targets: EASY trucks, MEDIUM armour under AAA, HARD armour under SHORAD"),
    dict(id="ew-1", name="EW-1 TSALKA", kind="ew", side="blue",
         shape=("rect", (41.69, 44.15), 20000, 15000),
         purpose="GPS jamming: jammer on the Trialeti slope, coordinate targets on the Tsalka plateau"),
    dict(id="hz-1", name="HZ-1 LIAKHVI", kind="hot_zone", side="blue",
         shape=("circle", (42.38, 44.10), 20 * NM),
         purpose="HOT ZONE: red CAP, SHORAD and targets that fight back. AWACS Overlord 1 on 251.5"),
    dict(id="ll-1", name="LL-1 KOLKHETI", kind="low_level", side="blue",
         shape=("route", [g[1] for g in LL1_GATES], 3000),
         purpose="Low-level route, 8 gates, 69 nm: below 500 ft AGL, 420 kts"),
    dict(id="cs-1", name="CS-1 BAKHMARO", kind="csar", side="blue",
         shape=("circle", (41.83, 42.40), 15000),
         purpose="CSAR: forested Meskheti ridges. Beacon 350 kHz"),
    dict(id="pt-1", name="PT-1 SENAKI", kind="pattern", side="blue",
         shape=("circle", SENAKI_ARP, 5 * NM),
         purpose="Circuits and landings, RWY 09/27; every landing graded"),
    dict(id="fd-1", name="FD-1 KOBULETI", kind="ship_deck", side="blue",
         shape=("circle", (42.00, 41.52), 8 * NM),
         purpose="Frigate deck landings: FFG-7 Perry and Arleigh Burke under way"),
    dict(id="as-2", name="AS-2 SUKHUMI", kind="anti_ship", side="all",
         shape=("rect", (42.72, 40.60), 60000, 40000),
         purpose="Escorted convoy, red: Krivak, Neustrashimy and a Tor-armed corvette guarding merchants. Weapons free"),
    # ---- red
    dict(id="s-11", name="S-11 KURSAVKA", kind="threat", side="red",
         shape=("rect", (44.55, 42.48), 60000, 45000),
         purpose="LIVE SEAD/IADS: EWR, Hawk, Patriot, NASAMS, IRIS-T SLM, Roland, Gepard. Weapons free, networked"),
    dict(id="r-15", name="R-15 EDISSEYA", kind="air_to_ground", side="red",
         shape=("rect", (43.968, 44.541), 15000, 8000),
         purpose="Tiered targets: EASY trucks, MEDIUM armour under AAA, HARD armour under SHORAD"),
    dict(id="ew-11", name="EW-11 TEREK", kind="ew", side="red",
         shape=("rect", (43.578, 44.89), 20000, 15000),
         purpose="GPS jamming: jammer on the Terek ridge, coordinate targets on the plain north of it"),
    dict(id="hz-11", name="HZ-11 ZELENCHUK", kind="hot_zone", side="red",
         shape=("circle", (43.95, 41.75), 20 * NM),
         purpose="HOT ZONE: blue CAP, SHORAD and targets that fight back. AWACS Focus 1 on 124.5"),
    dict(id="ll-11", name="LL-11 PSEKUPS", kind="low_level", side="red",
         shape=("route", [g[1] for g in LL11_GATES], 3000),
         purpose="Low-level route, 8 gates, 78 nm Krymsk to Maykop: below 500 ft AGL, 420 kts"),
    dict(id="cs-11", name="CS-11 CHEGEM", kind="csar", side="red",
         shape=("circle", (43.44, 43.34), 13000),
         purpose="CSAR: forested ridges between the Baksan and Chegem gorges. Beacon 400 kHz"),
    dict(id="pt-11", name="PT-11 MINERALNYE VODY", kind="pattern", side="red",
         shape=("circle", MINVODY_ARP, 5 * NM),
         purpose="Circuits and landings, RWY 12/30; every landing graded"),
    dict(id="fd-11", name="FD-11 UTRISH", kind="ship_deck", side="red",
         shape=("circle", (44.52, 37.43), 8 * NM),
         purpose="Frigate deck landings: Neustrashimy and Project 22160 under way"),
    dict(id="as-3", name="AS-3 TAMAN", kind="anti_ship", side="all",
         shape=("rect", (44.57, 36.80), 60000, 40000),
         purpose="Escorted convoy, NATO: Arleigh Burke and Perry guarding merchants. Weapons free"),
]
_BY_ID.update({s["id"]: s for s in SECTORS})

ZONES += [
    ("S1-SHORAD", (41.57, 43.52), 250), ("S1-SA6", (41.53, 43.38), 250), ("S1-SA3", (41.44, 43.56), 250),
    ("S1-SA11", (41.42, 43.39), 250), ("S1-EWR", (41.335, 43.42), 250), ("S1-SA2", (41.34, 43.66), 300),
    ("S1-SA10", (41.26, 43.40), 300),
    ("R4-EASY", (41.515, 45.285), 150), ("R4-MEDIUM", (41.51, 45.33), 150), ("R4-HARD", (41.505, 45.375), 150),
    ("EW1-JAMMER", (41.738, 44.10), 150), ("EW1-TARGETS", (41.685, 44.15), 200),
    ("HZ1-T1", (42.175, 43.915), 200), ("HZ1-T2", (42.12, 44.12), 200), ("HZ1-T3", (42.19, 44.35), 200),
    ("HZ1-T4", (42.28, 44.12), 200), ("HZ1-T5", (42.31, 43.86), 200),
    *[(z, ll, 1000) for z, ll, _ in LL1_GATES],
    ("S11-SHORAD", (44.39, 42.80), 250), ("S11-ROLAND", (44.50, 42.65), 250), ("S11-IRIST", (44.55, 42.75), 250),
    ("S11-NASAMS", (44.55, 42.38), 250), ("S11-HAWK", (44.62, 42.55), 300), ("S11-EWR", (44.66, 42.35), 250),
    ("S11-PATRIOT", (44.70, 42.18), 300),
    ("R15-EASY", (43.974, 44.497), 150), ("R15-MEDIUM", (43.968, 44.541), 150), ("R15-HARD", (43.962, 44.585), 150),
    ("EW11-JAMMER", (43.537, 44.87), 150), ("EW11-TARGETS", (43.618, 44.89), 200),
    ("HZ11-T1", (43.98, 41.78), 200), ("HZ11-T2", (44.10, 41.70), 200), ("HZ11-T3", (43.95, 41.55), 200),
    ("HZ11-T4", (43.80, 41.75), 200), ("HZ11-T5", (44.08, 42.10), 200),
    *[(z, ll, 1000) for z, ll, _ in LL11_GATES],
]

# Air defence networks: (zone, system, site name, battery heading). `side`
# is whose kit it is; the other side attacks it.
IADS = [
    dict(id="s1-iads", sector="s-1", name="S-1 Akhalkalaki IADS", side="red", sites=[
        ("S1-EWR", "ewr_55g6", "Javakheti EWR", 0),
        ("S1-SA10", "sa10", "SA-10 battery", 0),
        ("S1-SA2", "sa2", "SA-2 site", 0),
        ("S1-SA11", "sa11", "SA-11 battery", 0),
        ("S1-SA3", "sa3", "SA-3 site", 0),
        ("S1-SA6", "sa6", "SA-6 battery", 0),
        ("S1-SHORAD", "pantsir", "Pantsir point defence", 0),
    ]),
    dict(id="s11-iads", sector="s-11", name="S-11 Kursavka IADS", side="blue", sites=[
        ("S11-EWR", "ewr_fps117", "Kursavka EWR", 180),
        ("S11-PATRIOT", "patriot", "Patriot battery", 180),
        ("S11-HAWK", "hawk", "Hawk battery", 180),
        ("S11-NASAMS", "nasams", "NASAMS battery", 180),
        ("S11-IRIST", "iris_t", "IRIS-T SLM battery", 180),
        ("S11-ROLAND", "roland", "Roland section", 180),
        ("S11-SHORAD", "gepard", "Gepard section", 180),
    ]),
]

# Tiered target areas: zones <prefix>-EASY/-MEDIUM/-HARD; `targets` is whose kit.
TIERED = [
    dict(sector="r-4", prefix="R4", sid="r4", name="R-4 Gareji", targets="red"),
    dict(sector="r-15", prefix="R15", sid="r15", name="R-15 Edisseya", targets="blue"),
]

# GPS jammers; `side` is the jammer's own side (a red jammer denies blue pilots).
JAMMERS = [
    dict(id="ew1", sector="ew-1", name="EW-1 Tsalka jammer", side="red", zone="EW1-JAMMER",
         targets_zone="EW1-TARGETS", gps="jam", glonass="jam", radio="off", radius_nm=12),
    dict(id="ew11", sector="ew-11", name="EW-11 Terek jammer", side="blue", zone="EW11-JAMMER",
         targets_zone="EW11-TARGETS", gps="jam", glonass="jam", radio="off", radius_nm=12),
]

# Hot zones: ground sites (zone, composition, name) and an AWACS on the
# players' side, orbiting well clear of the zone.
HOT_ZONES = [
    dict(id="hz1", sector="hz-1", name="HZ-1 Liakhvi", ai_side="red",
         cap_types=["MiG-29S", "Su-27", "J-11A"],
         sites=[("HZ1-T1", "armor_modern", "Armour at Kareli"), ("HZ1-T2", "sam_modern", "Pantsir / Tor site"),
                ("HZ1-T3", "artillery", "Artillery battery"), ("HZ1-T4", "shorad", "SHORAD"),
                ("HZ1-T5", "hq", "Command post")],
         awacs=dict(typ="E-3A", callsign="Overlord", n=1, at=(41.75, 42.90), heading=90, freq=251.5)),
    dict(id="hz11", sector="hz-11", name="HZ-11 Zelenchuk", ai_side="blue",
         cap_types=["F-16C_50", "F-15C", "FA-18C_hornet"],
         sites=[("HZ11-T1", "armor_modern", "Armour column"), ("HZ11-T2", "sam_modern", "IRIS-T site"),
                ("HZ11-T3", "artillery", "Artillery battery"), ("HZ11-T4", "shorad", "SHORAD"),
                ("HZ11-T5", "hq", "Command post")],
         awacs=dict(typ="A-50", callsign="Focus", n=1, at=(44.40, 41.10), heading=270, freq=124.5)),
]

ROUTES = [
    dict(id="ll1", sector="ll-1", name="LL-1 Kolkheti", side="blue", gates=[(z, n) for z, _, n in LL1_GATES]),
    dict(id="ll11", sector="ll-11", name="LL-11 Psekups", side="red", gates=[(z, n) for z, _, n in LL11_GATES]),
]

CSAR_AREAS = [
    dict(id="cs1", sector="cs-1", name="CS-1 Bakhmaro", side="blue", khz=350),
    dict(id="cs11", sector="cs-11", name="CS-11 Chegem", side="red", khz=400),
]

# Frigates: each steams `leg_nm` from `at` along `heading` and back.
DECKS = [
    dict(id="fd1-perry", name="FFG-7 Perry", typ="PERRY", side="blue", at=(42.00, 41.453), heading=90,
         speed_kts=10, leg_nm=4, tacan=(41, "X", "FFG")),
    dict(id="fd1-burke", name="DDG Arleigh Burke", typ="USS_Arleigh_Burke_IIa", side="blue", at=(41.965, 41.587),
         heading=270, speed_kts=14, leg_nm=4, tacan=(42, "X", "DDG")),
    dict(id="fd11-neustrash", name="Neustrashimy", typ="NEUSTRASH", side="red", at=(44.52, 37.36), heading=90,
         speed_kts=10, leg_nm=4, tacan=(43, "X", "NEU")),
    dict(id="fd11-p22160", name="Project 22160 corvette", typ="CHAP_Project22160", side="red", at=(44.485, 37.50),
         heading=270, speed_kts=12, leg_nm=4, tacan=(44, "X", "PRJ")),
]

# Escorted convoys: `targets` is whose ships; (type, x ahead, y right) metres.
ESCORTED = [
    dict(id="as2-convoy", sector="as-2", name="AS-2 Sukhumi - Escorted Convoy", targets="red",
         start=(42.60, 40.93), end=(42.84, 40.28),
         ships=[("Dry-cargo ship-1", 0, 0), ("Dry-cargo ship-2", -700, 0), ("ELNYA", -1400, 0),
                ("REZKY", 1500, 800), ("NEUSTRASH", -700, -1200), ("CHAP_Project22160_TorM2KM", -2200, 700)],
         note="Merchants under a Krivak, a Neustrashimy and a Tor-armed corvette: weapons free, the missile trainer protects you."),
    dict(id="as3-convoy", sector="as-3", name="AS-3 Taman - Escorted Convoy", targets="blue",
         start=(44.45, 37.13), end=(44.68, 36.58),
         ships=[("HandyWind", 0, 0), ("Ship_Tilde_Supply", -700, 0), ("Dry-cargo ship-2", -1400, 0),
                ("USS_Arleigh_Burke_IIa", 1500, 800), ("PERRY", -700, -1200)],
         note="Merchants under an Arleigh Burke and a Perry: weapons free, the missile trainer protects you."),
]
