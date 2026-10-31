# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""The ranges marked on the ground, the way a real range is: rings of tyres
round the bomb circle and the target arrays, a tyre foul line on the strafe
pit, flags round the coordinate targets and along the gunnery lane, tyre
pads, cones and a windsock for the helicopter courses.

They are mission statics (the builder writes them into the .miz), neutral
(UN Peacekeepers) so a bomb on a tyre is not a team kill, and hidden on the
F10 map so they do not clutter it. They do not respawn: a marker a bomb
destroys stays destroyed until the next mission start. All the types are in
DCS's own static list (Scripts/Database/db_countries.lua): White_Tyre,
Black_Tyre_RF, Black_Tyre_WF, Red_Flag, White_Flag, Airshow_Cone, Windsock.
"""
import math

import layout

COUNTRY_ID = 82
COUNTRY = "UN Peacekeepers"


def _zone(name):
    return next(ll for z, ll, _ in layout.ZONES if z == name)


def _ring(base, radius_m, n, typ):
    return [(typ, layout.at(base, radius_m * math.cos(2 * math.pi * i / n), radius_m * math.sin(2 * math.pi * i / n)))
            for i in range(n)]


def _along(base, heading_deg, ahead_m, right_m=0.0):
    """`ahead_m` along `heading_deg` from `base`, `right_m` to its right."""
    h = math.radians(heading_deg)
    return layout.at(base, ahead_m * math.cos(h) - right_m * math.sin(h), ahead_m * math.sin(h) + right_m * math.cos(h))


def _square(base, half_m, typ):
    return [(typ, layout.at(base, n * half_m, e * half_m)) for n, e in ((1, 1), (1, -1), (-1, 1), (-1, -1))]


def bomb_circle(zone, rings=((25, 8), (50, 12), (100, 20))):
    """Concentric rings of white tyres, the centre is the engine's target."""
    c = _zone(zone)
    return [m for r, n in rings for m in _ring(c, r, n, "White_Tyre")]


def strafe_pit(zone, heading_deg, foul_line_m=610):
    """The foul line across the run-in, red-flagged tyres; a white flag each
    side of the target panels."""
    c = _zone(zone)
    back = (heading_deg + 180) % 360
    foul = [("Black_Tyre_RF", _along(c, back, foul_line_m, off)) for off in (-75, -50, -25, 0, 25, 50, 75)]
    return foul + [("White_Flag", _along(c, heading_deg, 0, off)) for off in (-45, 45)]


def target_array(zone, radius_m=50):
    """A tyre ring round the array, a white flag on its north side."""
    c = _zone(zone)
    return _ring(c, radius_m, 12, "White_Tyre") + [("White_Flag", layout.at(c, radius_m + 15, 0))]


def coord_targets(zone, half_m=200):
    return _square(_zone(zone), half_m, "Red_Flag")


def convoy_ends(start_zone, end_zone):
    return [("Airshow_Cone", layout.at(_zone(z), 0, e)) for z in (start_zone, end_zone) for e in (-12, 12)]


def gunnery_lane(zone, heading_deg, ranges_m=(800, 1200, 1600)):
    """The firing line across the lane, white-flagged tyres, and a red flag
    beside each target range."""
    c = _zone(zone)
    line = [("Black_Tyre_WF", _along(c, heading_deg, 0, off)) for off in (-40, -20, 0, 20, 40)]
    return line + [("Red_Flag", _along(c, heading_deg, r, -60)) for r in ranges_m]


def helo_course(p):
    """Tyre pad and windsock, the confined LZ's corners, tyres on the
    pinnacle, cones round each pickup and drop zone."""
    out = _ring(_zone(f"{p}-PAD"), 6, 8, "White_Tyre")
    out.append(("Windsock", layout.at(_zone(f"{p}-PAD"), 0, 50)))
    out += _square(_zone(f"{p}-CONFINED"), 15, "Black_Tyre_RF")
    out += _square(_zone(f"{p}-PINNACLE"), 5, "White_Tyre")
    for z, half in (("SLING1-PICKUP", 10), ("SLING1-DZ", 15), ("SLING2-PICKUP", 10), ("SLING2-DZ", 15),
                    ("TROOPS-PICKUP", 15), ("TROOPS-LZ", 25)):
        out += _square(_zone(f"{p}-{z}"), half, "Airshow_Cone")
    return out


def all_markers():
    """[(name, type, (lat, lon))] for every marker on the range."""
    groups = [
        ("R7201 bomb", bomb_circle("R7201-BOMB")),
        ("R7201 strafe", strafe_pit("R7201-STRAFE", layout.FDM_STRAFE_HDG)),
        ("R7201 coord", coord_targets("R7201-JDAM", 120)),
        ("TN1 array", target_array("TN1-ARRAY")),
        ("TN1 convoy", convoy_ends("TN1-CONVOY", "TN1-CONVOY-END")),
        ("TN1 coord", coord_targets("TN1-JDAM")),
        ("TN2 easy", target_array("TN2-EASY")),
        ("TN2 medium", target_array("TN2-MEDIUM")),
        ("TN2 hard", target_array("TN2-HARD")),
        ("EW1 coord", coord_targets("EW1-TARGETS")),
        ("G1 lane", gunnery_lane("G1-LANE", 60)),
        ("H1 course", helo_course("H1")),
    ]
    return [(f"Marker {g} {i + 1}", typ, ll) for g, ms in groups for i, (typ, ll) in enumerate(ms)]
