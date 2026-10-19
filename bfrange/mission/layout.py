# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""The range's layout: the Mariana Islands Range Complex, as the US flies it.

The real complex is a set of special use airspaces around Guam and the
Northern Marianas: warning areas and ATCAAs over the sea for air-to-air and
large-force work, the one live bombing range on Farallon de Medinilla
(R-7201), refuelling tracks with published entry and exit lines, and the
military lease area on Tinian for ground and helicopter training. The range
uses that airspace for what it is used for in life, and puts the jobs it has
no real home for (threat replication, jamming, a contested island) on the
islands that have room for them.

Blue only. Everyone flies from Guam (Andersen AFB, Won Pat) or the decks,
the way a real exercise launches from Andersen and works up the chain. The
opposition is AI: target arrays, threat emitters, an IADS, a hot zone CAP.

Every block of the real airspace is a sector with a job: the warning areas
and ATCAAs for air-to-air, BVR, duels, large-force and maritime strike,
R-7201 and R-7201A for bombing, the published AR tracks for refuelling. The
boundaries are the published ones (they come out on whole arc minutes from
the Marianas SUA mission's drawings); W-517 is traced from the NAVFAC
"Airspace in the Mariana Islands Range Complex" chart. Those are drawn as
thin closed lines (`airspace=True`), the range's smaller sectors inside them
as filled outlines. Where two sectors overlap the engine reports the
smaller one, so a restricted area inside a warning area reads the way it
does on a chart.

Shapes:
    ("rect", (lat, lon), east_west_m, north_south_m)
    ("circle", (lat, lon), radius_m)
    ("poly", [(lat, lon), ...])                            real airspace boundaries
    ("track", (lat, lon), heading_deg, leg_m, width_m)    a tanker race-track
    ("route", [(lat, lon), ...], width_m)                  a low-level route's corridor
"""

import math

NM = 1852.0

THEATRE = "MarianaIslands"

# Airfields (DCS reference points).
ANDERSEN = (13.5840, 144.9300)   # RWY 06/24 L/R
WON_PAT = (13.4834, 144.7960)    # RWY 06/24 L/R
FDM = (16.0170, 146.0600)        # Farallon de Medinilla, the middle of the island
FDM_STRAFE_HDG = 209             # strafe run-in along the island from the NNE, rounds into the sea

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

# ------------------------------------------------------------------ real airspace

W_517 = [(13.133, 144.500), (13.133, 144.700), (11.000, 144.700), (11.000, 143.000),
         (11.733, 143.000), (12.800, 144.500)]
ATCAA_1 = [(12.8333, 145.0000), (12.8333, 145.1667), (11.4167, 147.0000), (10.7869, 147.0038),
           (10.7866, 145.1692)]
ATCAA_2 = [(12.3333, 143.0000), (12.8333, 144.1667), (12.8333, 144.6833), (10.6974, 144.8281),
           (10.7667, 143.0128)]
ATCAA_3A = [(17.0000, 145.0833), (17.0000, 146.3333), (16.0000, 146.4167), (15.5000, 146.1667),
            (15.6667, 146.0000), (15.7000, 145.5833)]
ATCAA_3B = [(17.0000, 146.3334), (17.0000, 147.0000), (14.4167, 147.0000), (14.1167, 146.3167),
            (13.5833, 146.0000), (13.8500, 145.5667), (14.0834, 145.5666), (14.8333, 146.2667),
            (15.2333, 146.3333), (15.5000, 146.1667), (16.0000, 146.4167)]
ATCAA_3C = [(17.0000, 147.0000), (17.0000, 147.1667), (16.2500, 148.0000), (14.7500, 148.0000),
            (14.4167, 147.0000)]
ATCAA_5 = [(13.5833, 144.0000), (13.2500, 144.0000), (12.0000, 141.5000), (13.7000, 141.5000),
           (14.0000, 142.8333)]
ATCAA_6 = [(15.4167, 144.2500), (15.4167, 146.4167), (13.0000, 146.4167), (13.0000, 144.2500)]

# Refuelling tracks as published: the racetrack's centre, its long axis and
# footprint (from the SUA mission's drawings), and the entry (ARIP to ARCP)
# and exit lines. Tankers join at the ARCP; receivers fly the entry line.
AR_TRACKS = {
    "rio": dict(centre=(11.2741, 145.5828), heading=346, length_m=54118, width_m=31965,
                entry=[(12.8333, 145.1667), (12.0000, 145.2500), (11.4825, 145.3743), (11.0000, 145.5000)],
                exit=[(12.8333, 145.0000), (12.0000, 145.2500)]),
    "mirage": dict(centre=(16.4841, 146.0575), heading=354, length_m=55583, width_m=29307,
                   entry=[(15.6666, 145.7501), (16.0008, 146.2506), (16.2499, 146.2203), (16.7501, 146.1665)],
                   exit=[(15.6666, 145.9833), (16.0008, 146.2506)]),
    "luxor": dict(centre=(11.2482, 143.4020), heading=1, length_m=55242, width_m=33480,
                  entry=[(12.8314, 144.4981), (11.9981, 143.2481), (11.4981, 143.2481), (10.9981, 143.2481)],
                  exit=[(12.0000, 143.2500), (12.8333, 144.3333)]),
}


def _track(name):
    """A published track's racetrack: legs along the long axis, ends as wide
    as the footprint, so the drawn track fills the published box."""
    t = AR_TRACKS[name]
    leg = t["length_m"] - t["width_m"]
    h = math.radians(t["heading"])
    start = at(t["centre"], -math.cos(h) * leg / 2, -math.sin(h) * leg / 2)
    return ("track", start, t["heading"], leg, t["width_m"])


def at(base, north_m=0.0, east_m=0.0):
    """(lat, lon) `north_m`/`east_m` from `base` (a sector id or a lat/lon)."""
    lat, lon = centre(base) if isinstance(base, str) else base
    return (lat + north_m / 111_132.0,
            lon + east_m / (111_320.0 * math.cos(math.radians(lat))))


# ------------------------------------------------------------------ sectors

# Low-level route around Guam's coast, counter-clockwise from Ritidian Point.
LL1_GATES = [("LL1-G1", (13.660, 144.862), "RITIDIAN"), ("LL1-G2", (13.602, 144.958), "PATI"),
             ("LL1-G3", (13.428, 144.812), "PAGO"), ("LL1-G4", (13.352, 144.782), "TALOFOFO"),
             ("LL1-G5", (13.268, 144.762), "INARAJAN"), ("LL1-G6", (13.228, 144.650), "COCOS"),
             ("LL1-G7", (13.300, 144.640), "UMATAC"), ("LL1-G8", (13.447, 144.612), "OROTE")]

# id, name, kind, for ("blue" | "red" | "all"), shape, one line of what is in it
SECTORS = [
    # ---- the warning areas and ATCAAs: every block has a job, as in life
    dict(id="w-517", name="W-517", kind="air_to_air", side="all", airspace=True,
         shape=("poly", W_517),
         purpose="Warning area SW of Guam: BFM / ACM set-ups, live missile shots. Floor 5,000 ft"),
    dict(id="atcaa-2", name="ATCAA 2 (W-12)", kind="duel", side="all", airspace=True,
         shape=("poly", ATCAA_2),
         purpose="Duels round W-517: 1v1 / 2v2 by agreement, missile trainer on, guns are real"),
    dict(id="atcaa-1", name="ATCAA 1 (W-11)", kind="bvr", side="all", airspace=True,
         shape=("poly", ATCAA_1),
         purpose="BVR presentations, Fox 1 / Fox 3 missile defence"),
    dict(id="atcaa-5", name="ATCAA 5", kind="air_to_air", side="all", airspace=True,
         shape=("poly", ATCAA_5),
         purpose="BFM west of Guam: set-ups and dogfights away from the W-517 traffic. Floor 5,000 ft"),
    dict(id="atcaa-6", name="ATCAA 6", kind="air_to_air", side="all", airspace=True,
         shape=("poly", ATCAA_6),
         purpose="Over the islands: intercepts and DCA CAP above FL150; the ground ranges work below it"),
    dict(id="atcaa-3a", name="ATCAA 3A (W-13A)", kind="bvr", side="all", airspace=True,
         shape=("poly", ATCAA_3A),
         purpose="Large-force exercises: strike packages into R-7201 against red air presentations"),
    dict(id="atcaa-3b", name="ATCAA 3B (W-13B)", kind="anti_ship", side="all", airspace=True,
         shape=("poly", ATCAA_3B),
         purpose="Maritime strike: the escorted convoy (AS-2) steams here; SINKEX and harpoon shots"),
    dict(id="atcaa-3c", name="ATCAA 3C (W-13C)", kind="bvr", side="all", airspace=True,
         shape=("poly", ATCAA_3C),
         purpose="Intercepts east of the chain: Fox 3 presentations at range"),

    # ---- the published refuelling tracks
    dict(id="ar-rio", name="AR RIO", kind="aar", side="all", shape=_track("rio"), tag="KC-135 51Y 251.0 FL250 300 KIAS",
         purpose="KC-135 boom, FL250 at 300 KIAS, TACAN 51Y, 251.0. Enter on the RIO line from W-11"),
    dict(id="ar-mirage", name="AR MIRAGE", kind="aar", side="all", shape=_track("mirage"), tag="MPRS 52Y 252.0 FL210 270 KIAS",
         purpose="KC-135MPRS basket, FL210 at 270 KIAS, TACAN 52Y, 252.0. Next to FDM: tank before or after R-7201"),
    dict(id="ar-luxor", name="AR LUXOR", kind="aar", side="all", shape=_track("luxor"), tag="KC-135 54Y 254.0 FL240 300 KIAS",
         purpose="KC-135 boom, FL240 at 300 KIAS, TACAN 54Y, 254.0. Enter on the LUXOR line past W-517"),
    dict(id="ar-shell", name="AR SHELL", kind="aar", side="all", tag="KC-130 53Y 253.0 14,000 ft 220 KIAS",
         shape=("track", (13.42, 144.42), 0, 20 * NM, 6 * NM),
         purpose="KC-130 basket, 14,000 ft at 220 KIAS for the slower jets (Harrier, A-4), TACAN 53Y, 253.0. West of Guam"),

    # ---- R-7201: Farallon de Medinilla, the live impact area, and its
    # 12 nm R-7201A round it for holding and run-ins
    dict(id="r-7201a", name="R-7201A", kind="air_to_ground", side="all", airspace=True,
         shape=("circle", FDM, 12 * NM),
         purpose="FDM approach: hold on the 12 nm ring, check the range clear, run in to R-7201"),
    dict(id="r-7201", name="R-7201 FDM", kind="air_to_ground", side="all",
         shape=("circle", FDM, 3 * NM),
         purpose="Farallon de Medinilla: bomb circle, strafe pit (run in from the NNE along the island), coordinate targets"),

    # ---- Tinian, the military lease area: north to south, CAS, tiers, jamming
    dict(id="tn-1", name="TN-1 NORTH FIELD", kind="tactical", side="all",
         shape=("rect", (15.075, 145.635), 7000, 5000),
         purpose="Tactical array (laser 1688), convoy on the North Field runways, JDAM targets. JTAC Axeman 133.0 AM"),
    dict(id="tn-2", name="TN-2 TINIAN MLA", kind="air_to_ground", side="all",
         shape=("rect", (15.035, 145.630), 7000, 4000),
         purpose="Tiered targets: EASY trucks, MEDIUM armour under AAA, HARD armour under SHORAD"),
    dict(id="ew-1", name="EW-1 TINIAN SOUTH", kind="ew", side="all",
         shape=("rect", (14.982, 145.630), 8000, 5000),
         purpose="GPS jamming: jammer on the Carolinas heights, coordinate targets by the old West Field"),

    # ---- threat replication on the empty and the small islands
    dict(id="r-3", name="R-3 AGUIJAN", kind="threat", side="all",
         shape=("circle", (14.853, 145.556), 3500),
         purpose="SA-8 threat emitter, radar on / weapons hold. Instructors can spawn more SAMs"),
    dict(id="s-1", name="S-1 ROTA", kind="threat", side="all",
         shape=("rect", (14.160, 145.205), 20000, 13000),
         purpose="LIVE SEAD/IADS: EWR, SA-2, SA-3, SA-6, SA-11, SA-10, Pantsir. Weapons free, networked, trainer protects you"),
    dict(id="hz-1", name="HZ-1 SAIPAN", kind="hot_zone", side="all",
         shape=("circle", (15.200, 145.760), 7 * NM),
         purpose="HOT ZONE: red CAP, SHORAD and targets that fight back. AWACS Overlord 1 on 251.5"),

    # ---- Guam
    dict(id="h-1", name="H-1 ANDERSEN", kind="helo", side="all",
         shape=("rect", (13.565, 144.895), 9000, 11000),
         purpose="Pads at Andersen, confined LZ at Andersen South, pinnacle on Mt Santa Rosa, sling and troop courses"),
    dict(id="g-1", name="G-1 NORTHWEST FIELD", kind="gunnery", side="all",
         shape=("rect", (13.630, 144.858), 4500, 2500),
         purpose="Combined Arms gunnery lane on the Northwest Field range, firing east-north-east"),
    dict(id="pt-1", name="PT-1 ANDERSEN", kind="pattern", side="all",
         shape=("circle", ANDERSEN, 5 * NM),
         purpose="Overhead breaks and circuits, RWY 06/24; every landing on every field is graded"),
    dict(id="cs-1", name="CS-1 SOUTH GUAM", kind="csar", side="all",
         shape=("circle", (13.335, 144.690), 7000),
         purpose="CSAR: the jungle ridges around Mt Lamlam. Beacon 350 kHz"),
    dict(id="ll-1", name="LL-1 GUAM COAST", kind="low_level", side="all",
         shape=("route", [g[1] for g in LL1_GATES], 3000),
         purpose="Low-level route round the coast, Ritidian to Orote, 8 gates: below 500 ft AGL, 420 kts"),

    # ---- the sea
    dict(id="oparea", name="CV OPAREA", kind="carrier", side="all", tag="CVN 72X ICLS 11 | LHA 71X",
         shape=("circle", (13.100, 145.350), 20 * NM),
         purpose="CVN-72 TACAN 72X ICLS 11 Link-4 336.0 | LHA-1 TACAN 71X | recovery tanker A-6E 63Y 261.0"),
    dict(id="fd-1", name="FD-1 APRA", kind="ship_deck", side="all",
         shape=("circle", (13.450, 144.480), 7 * NM),
         purpose="Frigate deck landings off Apra Harbor: FFG-7 Perry and Arleigh Burke under way"),
    dict(id="as-1", name="AS-1 SINKEX", kind="anti_ship", side="all",
         shape=("rect", (12.100, 144.250), 50000, 35000),
         purpose="SINKEX hulks in W-517, undefended, steaming west. Spawn warships from the site or F10"),
    dict(id="as-2", name="AS-2 ESCORTED CONVOY", kind="anti_ship", side="all",
         shape=("rect", (15.000, 146.600), 60000, 40000),
         purpose="Escorted convoy in W-13B: Krivak, Neustrashimy and a Tor-armed corvette guarding merchants. Weapons free"),
]

_BY_ID = {s["id"]: s for s in SECTORS}


def centre(sector_id):
    shape = _BY_ID[sector_id]["shape"]
    if shape[0] in ("poly", "route"):
        pts = shape[1]
        return (sum(p[0] for p in pts) / len(pts), sum(p[1] for p in pts) / len(pts))
    return shape[1]


# Where the legend sits: the empty sea north-west of ATCAA 5, clear of every
# outline whichever way DCS grows the box from its anchor.
LEGEND_AT = (15.30, 141.70)

# Every sector's label is two short lines: its name, and its `tag` (the
# frequencies, for tankers and the carrier) or else its kind. DCS draws text
# at a fixed screen size, so at theatre zoom anything longer piles up; what
# each sector is for is on F10 > Range > Sectors and is announced when you
# fly in.


# ------------------------------------------------------------------ zones
#
# The trigger zones the config places things at, named <SECTOR>-<WHAT> so the
# Mission Editor list reads like the map. The islands are small, so most are
# placed by hand rather than by offset; the engine still moves a target that
# lands in water to dry ground within 600 m.

ZONES = [
    # R-7201 FDM: the island runs NNE-SSW (029), about 4 km by 600 m at its
    # widest. Bomb circle on its middle, strafe pit on the south end (run in
    # along the island from the NNE so the foul line is on land and the
    # rounds go into the sea), coordinate targets on the wide north end.
    # Placed on satellite imagery.
    ("R7201-BOMB", (16.0175, 146.0592), 150),
    ("R7201-STRAFE", (16.0061, 146.0520), 150),
    ("R7201-JDAM", (16.0270, 146.0625), 200),
    # TN-1 North Field: the convoy drives the length of the old runways
    ("TN1-ARRAY", (15.086, 145.627), 200),
    ("TN1-CONVOY", (15.068, 145.612), 150),
    ("TN1-CONVOY-END", (15.074, 145.652), 150),
    ("TN1-JDAM", (15.091, 145.645), 250),
    # TN-2: three tiers west to east across the island
    ("TN2-EASY", (15.038, 145.612), 150), ("TN2-MEDIUM", (15.035, 145.630), 150),
    ("TN2-HARD", (15.032, 145.648), 150),
    # EW-1
    ("EW1-JAMMER", (14.963, 145.645), 150), ("EW1-TARGETS", (14.990, 145.612), 200),
    # R-3 Aguijan
    ("R3-SAM", (14.853, 145.558), 300),
    # S-1 Rota: the battery spread along the island, the EWR on the Sabana
    ("S1-EWR", (14.142, 145.185), 250), ("S1-SA10", (14.155, 145.215), 300),
    ("S1-SA11", (14.168, 145.252), 250), ("S1-SA6", (14.135, 145.160), 250),
    ("S1-SA3", (14.182, 145.268), 250), ("S1-SA2", (14.148, 145.138), 300),
    ("S1-SHORAD", (14.160, 145.200), 250),
    # HZ-1 Saipan
    ("HZ1-T1", (15.125, 145.705), 200), ("HZ1-T2", (15.200, 145.760), 200),
    ("HZ1-T3", (15.155, 145.752), 200), ("HZ1-T4", (15.255, 145.800), 200),
    ("HZ1-T5", (15.230, 145.765), 200),
    # H-1 Andersen: pads on the base, the confined LZ in the Andersen South
    # urban site, the pinnacle on Mt Santa Rosa
    ("H1-PAD", (13.592, 144.915), 30),
    ("H1-CONFINED", (13.522, 144.874), 40),
    ("H1-PINNACLE", (13.537, 144.912), 30),
    ("H1-SLING1-PICKUP", (13.590, 144.905), 50),
    ("H1-SLING1-DZ", (13.560, 144.885), 50),
    ("H1-SLING2-PICKUP", (13.589, 144.908), 50),
    ("H1-SLING2-DZ", (13.545, 144.900), 60),
    ("H1-TROOPS-PICKUP", (13.588, 144.902), 60),
    ("H1-TROOPS-LZ", (13.525, 144.880), 80),
    # G-1 Northwest Field
    ("G1-LANE", (13.626, 144.845), 150),
    *[(z, ll, 1000) for z, ll, _ in LL1_GATES],
]

# Air defence networks: (zone, system, site name, battery heading). `side`
# is whose kit it is; the other side attacks it.
IADS = [
    dict(id="s1-iads", sector="s-1", name="S-1 Rota IADS", side="red", sites=[
        ("S1-EWR", "ewr_55g6", "Sabana EWR", 180),
        ("S1-SA10", "sa10", "SA-10 battery", 180),
        ("S1-SA2", "sa2", "SA-2 site", 180),
        ("S1-SA11", "sa11", "SA-11 battery", 180),
        ("S1-SA3", "sa3", "SA-3 site", 180),
        ("S1-SA6", "sa6", "SA-6 battery", 180),
        ("S1-SHORAD", "pantsir", "Pantsir point defence", 180),
    ]),
]

# Tiered target areas: zones <prefix>-EASY/-MEDIUM/-HARD; `targets` is whose kit.
TIERED = [
    dict(sector="tn-2", prefix="TN2", sid="tn2", name="TN-2 Tinian MLA", targets="red"),
]

# GPS jammers; `side` is the jammer's own side (a red jammer denies blue pilots).
JAMMERS = [
    dict(id="ew1", sector="ew-1", name="EW-1 Tinian jammer", side="red", zone="EW1-JAMMER",
         targets_zone="EW1-TARGETS", gps="jam", glonass="jam", radio="off", radius_nm=5),
]

# Hot zones: ground sites (zone, composition, name) and an AWACS on the
# players' side, orbiting well clear of the zone.
HOT_ZONES = [
    dict(id="hz1", sector="hz-1", name="HZ-1 Saipan", ai_side="red",
         cap_types=["J-11A", "Su-27", "MiG-29S"],
         sites=[("HZ1-T1", "armor_modern", "Armour at Aslito"), ("HZ1-T2", "sam_modern", "Pantsir / Tor site"),
                ("HZ1-T3", "artillery", "Artillery battery"), ("HZ1-T4", "shorad", "SHORAD at Marpi"),
                ("HZ1-T5", "hq", "Command post")],
         awacs=dict(typ="E-3A", callsign="Overlord", n=1, at=(14.40, 144.55), heading=30, freq=251.5)),
]

ROUTES = [
    dict(id="ll1", sector="ll-1", name="LL-1 Guam Coast", side="blue", gates=[(z, n) for z, _, n in LL1_GATES]),
]

CSAR_AREAS = [
    dict(id="cs1", sector="cs-1", name="CS-1 South Guam", side="blue", khz=350),
]

# Frigates: each steams `leg_nm` from `at` along `heading` and back.
DECKS = [
    dict(id="fd1-perry", name="FFG-7 Perry", typ="PERRY", side="blue", at=(13.47, 144.46), heading=0,
         speed_kts=10, leg_nm=4, tacan=(41, "X", "FFG")),
    dict(id="fd1-burke", name="DDG Arleigh Burke", typ="USS_Arleigh_Burke_IIa", side="blue", at=(13.43, 144.50),
         heading=180, speed_kts=14, leg_nm=4, tacan=(42, "X", "DDG")),
]

# Escorted convoys: `targets` is whose ships; (type, x ahead, y right) metres.
ESCORTED = [
    dict(id="as2-convoy", sector="as-2", name="AS-2 Escorted Convoy", targets="red",
         start=(14.88, 146.82), end=(15.12, 146.38),
         ships=[("Dry-cargo ship-1", 0, 0), ("Dry-cargo ship-2", -700, 0), ("ELNYA", -1400, 0),
                ("REZKY", 1500, 800), ("NEUSTRASH", -700, -1200), ("CHAP_Project22160_TorM2KM", -2200, 700)],
         note="Merchants under a Krivak, a Neustrashimy and a Tor-armed corvette: weapons free, the missile trainer protects you."),
]
