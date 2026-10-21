"""Draw the dashboard's wireframe art: a carrier, an Su-27, an F/A-18, an
AH-64 and an M1 tank, each a rough 3D model projected to SVG line work.

Writes src/lib/wireframes.gen.ts: per view, a viewBox and three path strings,
near to far -- the far lines are drawn fainter, which is what makes a
wireframe read as a solid. src/lib/wireframe.ts colours and fades them; the
models stay simple on purpose, they are background art at low opacity.

Units are metres: x forward, y right (starboard), z up.

    python bfweb/scripts/gen-wireframes.py
"""

import math
from pathlib import Path

import numpy as np

OUT = Path(__file__).resolve().parents[1] / "src" / "lib" / "wireframes.gen.ts"

Line = list  # a polyline: a list of (x, y, z)


# ── building blocks ─────────────────────────────────────────────────────────

def ellipse(cy, cz, a, b, n, a0=0.0, a1=2 * math.pi, closed=True):
    count = n if closed else n + 1
    step = (a1 - a0) / n
    return [(cy + a * math.cos(a0 + i * step), cz + b * math.sin(a0 + i * step)) for i in range(count)]


def superellipse(cy, cz, a, b, p, n):
    pts = []
    for i in range(n):
        t = 2 * math.pi * i / n
        c, s = math.cos(t), math.sin(t)
        pts.append((cy + a * math.copysign(abs(c) ** (2 / p), c), cz + b * math.copysign(abs(s) ** (2 / p), s)))
    return pts


def loft(stations, section, closed=True, rings=True, stringers=True, every=1):
    """Rings at each station and stringers joining them. `section(x)` gives
    the (y, z) points around the body at station x; a point section (the tip
    of a nose) collapses the stringers onto it."""
    secs = [[(x, y, z) for (y, z) in section(x)] for x in stations]
    out: list[Line] = []
    if rings:
        for s in secs:
            if len(set(s)) > 1:
                out.append(s + [s[0]] if closed else s)
    if stringers:
        n = max(len(s) for s in secs)
        for i in range(0, n, every):
            out.append([s[i % len(s)] for s in secs])
    return out


def mirror(lines: list[Line]) -> list[Line]:
    return lines + [[(x, -y, z) for (x, y, z) in l] for l in lines]


def poly(pts, closed=True) -> Line:
    pts = list(pts)
    return pts + [pts[0]] if closed else pts


def lerp(a, b, t):
    return tuple(a[i] + (b[i] - a[i]) * t for i in range(len(a)))


def circle_xz(cx, cy, cz, r, n=16) -> Line:
    return poly([(cx + r * math.cos(2 * math.pi * i / n), cy, cz + r * math.sin(2 * math.pi * i / n)) for i in range(n)])


def circle_xy(cx, cy, cz, r, n=24) -> Line:
    return poly([(cx + r * math.cos(2 * math.pi * i / n), cy + r * math.sin(2 * math.pi * i / n), cz) for i in range(n)])


def box(x0, x1, y0, y1, z0, z1) -> list[Line]:
    b = [(x0, y0, z0), (x1, y0, z0), (x1, y1, z0), (x0, y1, z0)]
    t = [(x, y, z1) for (x, y, _) in b]
    return [poly(b), poly(t)] + [[b[i], t[i]] for i in range(4)]


def prism(foot, z0, z1, top_scale=1.0) -> list[Line]:
    """A vertical prism over a footprint (x, y) list, its top shrunk toward the centre."""
    cx = sum(p[0] for p in foot) / len(foot)
    cy = sum(p[1] for p in foot) / len(foot)
    b = [(x, y, z0) for (x, y) in foot]
    t = [(cx + (x - cx) * top_scale, cy + (y - cy) * top_scale, z1) for (x, y) in foot]
    return [poly(b), poly(t)] + [[b[i], t[i]] for i in range(len(foot))]


def surface(edge_a, edge_b, ribs=4, spars=()) -> list[Line]:
    """A flat panel between two edges (leading, trailing), with the outline,
    `ribs` lines across and a line at each chord fraction in `spars`."""
    out = [edge_a, edge_b, [edge_a[0], edge_b[0]], [edge_a[-1], edge_b[-1]]]

    def at(edge, t):
        # point a fraction t along a polyline, by index
        f = t * (len(edge) - 1)
        i = min(int(f), len(edge) - 2)
        return lerp(edge[i], edge[i + 1], f - i)

    for k in range(1, ribs + 1):
        t = k / (ribs + 1)
        out.append([at(edge_a, t), at(edge_b, t)])
    for c in spars:
        out.append([lerp(at(edge_a, i / 12), at(edge_b, i / 12), c) for i in range(13)])
    return out


# ── models ──────────────────────────────────────────────────────────────────

def su27() -> list[Line]:
    L: list[Line] = []
    # Forward fuselage: a drooped cone into the cockpit section.
    def fwd(x):
        r = 0.88 * min(1.0, ((10.9 - x) / 5.2) ** 0.62)
        cz = -0.18 * max(0.0, (x - 4.5) / 6.4)
        return ellipse(0, cz, r, r * 1.04, 12)
    L += loft([10.9, 10.3, 9.4, 8.3, 7.1, 5.8, 4.4, 3.0, 1.6], fwd, every=2)
    # Canopy bubble.
    def canopy(x):
        t = (6.7 - x) / 3.6
        h = 0.58 * math.sin(math.pi * min(max(t, 0), 1)) ** 0.7
        return ellipse(0, 0.82, 0.42 * (0.4 + 0.6 * h / 0.58), max(h, 0.001), 6, 0, math.pi, closed=False)
    L += loft([6.7, 6.2, 5.4, 4.6, 3.8, 3.1], canopy, closed=False)
    # Centre body between the wings.
    def centre(x):
        t = (1.6 - x) / 9.6
        a = 1.0 + 0.5 * math.sin(math.pi * min(t * 1.4, 1)) - 0.5 * max(0, t - 0.7) / 0.3
        b = 0.62 - 0.22 * t
        return ellipse(0, 0.1, a, b, 14)
    L += loft([1.6, 0.0, -1.8, -3.6, -5.4, -7.0, -8.0], centre, every=2)
    # Tail stinger.
    L += loft([-8.0, -9.0, -10.0, -11.0], lambda x: ellipse(0, 0.05, 0.42 - 0.09 * (-8 - x), 0.4 - 0.09 * (-8 - x), 8))
    # Engine nacelles: square intakes rounding into the nozzles.
    def nacelle(x):
        t = (1.8 - x) / 11.4
        p = 4.5 - 2.5 * t
        return superellipse(1.3, -0.78, 0.55 + 0.07 * t, 0.62 - 0.02 * t, p, 12)
    nac = loft([1.8, 0.6, -1.5, -4.0, -6.5, -8.5, -9.6], nacelle, every=2)
    nac += loft([-9.6, -10.2], lambda x: ellipse(1.3, -0.78, 0.56, 0.56, 12), stringers=True, every=3)
    L += mirror(nac)
    # Wings, with the LERX blending in.
    lerx = [(4.8, 0.85, 0.05), (3.2, 1.25, 0.05), (1.5, 1.7, 0.03), (0.0, 2.2, 0.0)]
    le = [(0.0, 2.2, 0.0), (-1.4, 3.8, 0.0), (-2.9, 5.45, 0.0), (-4.3, 7.0, 0.0)]
    te = [(-7.2, 1.7, 0.0), (-6.9, 2.8, 0.0), (-6.6, 4.0, 0.0), (-6.1, 5.6, 0.0), (-5.6, 7.1, 0.0)]
    wing = surface(le, te[:4] + [te[4]], ribs=3, spars=(0.62,))
    wing += [lerx]
    # Wingtip missile.
    wing += loft([-2.6, -3.2, -5.6, -6.4], lambda x: ellipse(7.25, 0.0, 0.1, 0.1, 6), every=2)
    L += mirror(wing)
    # Horizontal stabilisers.
    L += mirror(surface([(-7.3, 1.95, -0.35), (-9.4, 4.5, -0.35)], [(-10.7, 1.95, -0.35), (-10.3, 4.5, -0.35)], ribs=1))
    # Twin vertical tails.
    fin = [(-6.4, 1.75, 0.35), (-8.9, 1.75, 3.6), (-10.1, 1.75, 3.6), (-9.9, 1.75, 0.35)]
    L += mirror([poly(fin), [(-9.2, 1.75, 0.35), (-9.6, 1.75, 3.55)], [(-7.6, 1.75, 1.95), (-9.95, 1.75, 1.95)]])
    # Ventral fins.
    L += mirror([poly([(-6.6, 1.7, -1.25), (-7.6, 1.7, -1.95), (-8.4, 1.7, -1.95), (-8.2, 1.7, -1.25)])])
    return L


def fa18() -> list[Line]:
    L: list[Line] = []
    def fwd(x):
        r = 0.82 * min(1.0, ((8.6 - x) / 4.6) ** 0.6)
        cz = -0.12 * max(0.0, (x - 3.5) / 5.1)
        return ellipse(0, cz, r, r * 1.05, 12)
    L += loft([8.6, 8.0, 7.1, 6.0, 4.8, 3.5, 2.2, 1.0], fwd)
    def canopy(x):
        t = (5.0 - x) / 2.9
        h = 0.55 * math.sin(math.pi * min(max(t, 0), 1)) ** 0.7
        return ellipse(0, 0.78, 0.4 * (0.4 + 0.6 * h / 0.55), max(h, 0.001), 6, 0, math.pi, closed=False)
    L += loft([5.0, 4.5, 3.8, 3.0, 2.1], canopy, closed=False)
    L += loft([1.0, -1.0, -3.0, -5.0, -6.6], lambda x: ellipse(0, 0.1, 1.2 - 0.05 * max(0, -x - 3), 0.62, 14), every=2)
    nac = loft([0.8, -1.5, -4.0, -6.4, -7.4], lambda x: superellipse(0.72, -0.55, 0.5, 0.5, 3.5 - 1.5 * (0.8 - x) / 8.2, 12), every=2)
    L += mirror(nac)
    lex = [(5.2, 0.6, 0.1), (3.5, 0.95, 0.08), (1.5, 1.4, 0.04), (-0.2, 2.0, 0.0)]
    le = [(-0.2, 2.0, 0.0), (-1.3, 4.1, 0.0), (-2.4, 6.15, 0.0)]
    te = [(-4.9, 1.9, 0.0), (-4.5, 4.0, 0.0), (-4.0, 6.15, 0.0)]
    wing = surface(le, te, ribs=2, spars=(0.6,)) + [lex]
    wing += loft([-1.2, -1.8, -4.0, -4.6], lambda x: ellipse(6.25, 0.0, 0.09, 0.09, 6), every=2)
    L += mirror(wing)
    L += mirror(surface([(-5.3, 1.45, -0.3), (-7.3, 3.4, -0.3)], [(-8.2, 1.45, -0.3), (-8.0, 3.4, -0.3)], ribs=1))
    cant = math.tan(math.radians(20))
    root_z, h = 0.55, 2.7
    tip_y = 1.1 + h * cant
    fin = [(-3.6, 1.1, root_z), (-5.9, tip_y, root_z + h), (-7.2, tip_y, root_z + h), (-6.6, 1.1, root_z)]
    L += mirror([poly(fin), [(-5.1, 1.1 + h / 2 * cant, root_z + h / 2), (-6.9, 1.1 + h / 2 * cant, root_z + h / 2)]])
    return L


def apache() -> list[Line]:
    L: list[Line] = []
    def body(x):
        t = (6.2 - x) / 7.7
        a = 0.35 + 0.55 * math.sin(math.pi * min(t * 1.2, 1)) ** 0.6
        b = 0.55 + 0.45 * math.sin(math.pi * min(t * 1.3, 1)) ** 0.5
        return superellipse(0, 0.15, a, b, 3.0, 12)
    L += loft([6.2, 5.6, 4.6, 3.4, 2.0, 0.6, -0.8, -1.6], body, every=2)
    # Tandem canopy: two faceted humps.
    for x0, x1, h in ((4.9, 3.4, 0.55), (3.3, 1.6, 0.7)):
        L += loft([x0, (x0 * 2 + x1) / 3, (x0 + x1 * 2) / 3, x1],
                  lambda x, x0=x0, x1=x1, h=h: ellipse(0, 0.95, 0.45, max(h * max(math.sin(math.pi * (x0 - x) / (x0 - x1)), 0.0) ** 0.7, 0.001), 6, 0, math.pi, closed=False),
                  closed=False)
    # Tail boom, rising to the fin.
    L += loft([-1.6, -3.5, -5.5, -7.5, -8.6], lambda x: ellipse(0, 0.35 + 0.06 * (-1.6 - x), 0.45 - 0.035 * (-1.6 - x), 0.42 - 0.03 * (-1.6 - x), 8), every=2)
    L += [poly([(-7.9, 0, 0.75), (-8.9, 0, 2.9), (-9.5, 0, 2.9), (-9.0, 0, 0.7)])]
    L += mirror([poly([(-8.4, 0.2, 0.55), (-8.6, 1.7, 0.55), (-9.2, 1.7, 0.55), (-9.1, 0.2, 0.55)])])
    # Tail rotor on the left of the fin.
    tr = (-9.1, -0.35, 2.35)
    L += [circle_xz(tr[0], tr[1], tr[2], 1.4, 20)]
    for a in (0, 90):
        r = math.radians(a + 25)
        L += [[(tr[0] - 1.4 * math.cos(r), tr[1], tr[2] - 1.4 * math.sin(r)), (tr[0] + 1.4 * math.cos(r), tr[1], tr[2] + 1.4 * math.sin(r))]]
    # Engines high on the sides.
    L += mirror(loft([0.9, 0.0, -1.2, -2.1], lambda x: ellipse(0.78, 1.05, 0.34, 0.34, 8), every=2))
    # Stub wings, rocket pods and Hellfire rails.
    L += mirror([poly([(0.7, 0.8, 0.25), (0.6, 2.6, 0.25), (-0.3, 2.6, 0.25), (-0.4, 0.8, 0.25)])])
    for y in (1.45, 2.35):
        L += mirror(loft([1.3, 0.9, -0.3, -0.7], lambda x, y=y: ellipse(y, -0.2, 0.22, 0.22, 8), every=2))
    # Mast, hub and the main rotor: four blades and the disc they sweep.
    L += [[(0.0, 0, 1.55), (0.0, 0, 2.35)], circle_xy(0.0, 0.0, 2.35, 7.3, 40)]
    for a in (20, 110, 200, 290):
        r = math.radians(a)
        tip = (7.3 * math.cos(r), 7.3 * math.sin(r), 2.35)
        side = (0.25 * -math.sin(r), 0.25 * math.cos(r), 0)
        L += [poly([(side[0], side[1], 2.35), (tip[0] + side[0], tip[1] + side[1], 2.35),
                    (tip[0] - side[0], tip[1] - side[1], 2.35), (-side[0], -side[1], 2.35)])]
    # Chin gun and the gear.
    L += [[(4.2, 0, -0.55), (5.3, 0, -0.6)]]
    for y in (1.05, -1.05):
        L += [[(1.6, y * 0.5, -0.4), (1.6, y, -1.25)], circle_xz(1.6, y, -1.35, 0.35, 12)]
    L += [[(-7.6, 0, 0.2), (-7.7, 0, -0.45)], circle_xz(-7.7, 0, -0.55, 0.15, 10)]
    return L


def tank() -> list[Line]:
    L: list[Line] = []
    # Hull: a sloped glacis, flat deck, sides.
    side = [(3.95, 0.75), (2.9, 1.25), (-3.85, 1.25), (-3.95, 0.7), (-3.5, 0.45), (3.4, 0.45)]
    for y in (1.75, -1.75):
        L += [poly([(x, y, z) for (x, z) in side])]
    for (x, z) in side:
        L += [[(x, 1.75, z), (x, -1.75, z)]]
    # Tracks: idler, sprocket, the run along the ground and the road wheels.
    for y in (1.55, -1.55):
        L += [poly([(3.75, y, 0.8), (3.0, y, 0.02), (-3.2, y, 0.02), (-3.9, y, 0.8), (-3.5, y, 1.0), (3.4, y, 1.0)])]
        for i in range(7):
            L += [circle_xz(2.65 - i * 0.92, y, 0.38, 0.33, 10)]
        L += [circle_xz(3.55, y, 0.78, 0.3, 10), circle_xz(-3.65, y, 0.78, 0.3, 10)]
    # Side skirts.
    L += mirror([poly([(3.7, 1.85, 0.6), (3.0, 1.85, 1.2), (-3.85, 1.85, 1.2), (-3.85, 1.85, 0.62)])])
    # Turret: the angular wedge, with a bustle at the back.
    foot = [(2.15, 0.0), (1.6, 1.25), (-2.2, 1.65), (-3.0, 1.4), (-3.0, -1.4), (-2.2, -1.65), (1.6, -1.25)]
    L += prism(foot, 1.25, 2.3, top_scale=0.9)
    # Gun, with its fume extractor.
    L += loft([2.0, 4.2, 7.7], lambda x: ellipse(0, 1.85, 0.13, 0.13, 8), every=2)
    L += loft([4.6, 5.4], lambda x: ellipse(0, 1.85, 0.19, 0.19, 8), every=2)
    L += loft([2.0, 2.6], lambda x: ellipse(0, 1.85, 0.32, 0.3, 8), every=2)
    # Commander's cupola and machine gun.
    L += [circle_xy(-0.7, 0.7, 2.3, 0.4, 12), circle_xy(-0.7, 0.7, 2.55, 0.36, 12)]
    L += [[(-0.7, 0.7, 2.62), (0.5, 0.7, 2.7)]]
    return L


def carrier() -> list[Line]:
    L: list[Line] = []
    deck_z, wl = 20.0, 0.0
    # Hull: stations from bow to stern, flaring from the waterline to the deck.
    def hull(x):
        if x > 120:
            t = (x - 120) / 48
            hw_wl, hw_dk = 20 * (1 - t) ** 0.9, 26 * (1 - t) ** 0.6
        elif x < -140:
            t = (-140 - x) / 28
            hw_wl, hw_dk = 20 - 8 * t, 24 - 4 * t
        else:
            hw_wl, hw_dk = 20.0, 25.0
        return [(-hw_wl, wl), (-hw_dk, deck_z - 2), (hw_dk, deck_z - 2), (hw_wl, wl)]
    st = [166, 150, 130, 100, 60, 20, -20, -60, -100, -140, -158, -166]
    secs = [[(x, y, z) for (y, z) in hull(x)] for x in st]
    for s in secs[1:]:
        L += [[s[0], s[1]], [s[2], s[3]]]
    for i in range(4):
        L += [[s[i] for s in secs]]
    L += [[secs[-1][1], secs[-1][0], secs[-1][3], secs[-1][2]]]
    # Flight deck: the overhanging outline with the angled deck to port.
    deck = [(168, 0), (150, 17), (100, 26), (40, 30), (-30, 33), (-120, 30), (-160, 22), (-168, 12),
            (-168, -14), (-140, -24), (-40, -38), (20, -42), (55, -40), (80, -28), (120, -22), (150, -16)]
    top = [(x, y, deck_z) for (x, y) in deck]
    L += [poly(top)]
    # Angled landing area, its centreline and the bow and waist catapults.
    L += [[(-166, -4, deck_z), (45, -38, deck_z)], [(-166, 8, deck_z), (60, -26, deck_z)], [(-150, -16, deck_z), (30, -44, deck_z)]]
    L += [[(160, 5, deck_z), (85, 12, deck_z)], [(160, -5, deck_z), (85, -11, deck_z)],
          [(60, -30, deck_z), (-10, -20, deck_z)], [(50, -38, deck_z), (-20, -30, deck_z)]]
    # Arresting wires.
    for i in range(4):
        x = -120 + i * 12
        y = -4 - (x + 166) * 34 / 211
        L += [[(x, y - 10, deck_z), (x + 2, y + 10, deck_z)]]
    # Elevators.
    for (x0, x1, y0, y1) in ((45, 25, 30, 44), (5, -15, 33, 47), (-70, -90, 30, 44), (-110, -130, -27, -40)):
        L += [poly([(x0, y0, deck_z), (x1, y0, deck_z), (x1, y1, deck_z), (x0, y1, deck_z)])]
    # Island, its upper decks, the mast and a radar.
    L += box(-40, -12, 24, 32, deck_z, deck_z + 18)
    L += box(-36, -18, 25, 31, deck_z + 18, deck_z + 26)
    L += [[(-27, 28, deck_z + 26), (-27, 28, deck_z + 48)], [(-27, 22, deck_z + 40), (-27, 34, deck_z + 40)],
          [(-30, 28, deck_z + 44), (-24, 28, deck_z + 44)]]
    L += [circle_xy(-27, 28, deck_z + 33, 4.5, 14)]
    for k in (deck_z + 6, deck_z + 12):
        L += [poly([(-40, 24, k), (-12, 24, k), (-12, 32, k), (-40, 32, k)])]
    # A few jets parked on deck: deltas with a tail.
    def jet(x, y, hdg):
        r = math.radians(hdg)
        c, s = math.cos(r), math.sin(r)
        pts = [(9, 0), (-3, 6), (-7, 6), (-5, 1.5), (-8, 3), (-9, 2.5), (-9, -2.5), (-8, -3), (-5, -1.5), (-7, -6), (-3, -6)]
        return poly([(x + px * c - py * s, y + px * s + py * c, deck_z + 0.6) for (px, py) in pts])
    for (x, y, h) in ((120, 14, 200), (105, 16, 200), (90, 18, 200), (-60, 22, 150), (-75, 22, 150),
                      (-95, 20, 150), (130, -10, 180), (-150, 14, 120)):
        L += [jet(x, y, h)]
    # The bow wave and the wake, as a few lines on the water.
    L += [[(168, 0, 0), (150, 36, 0), (120, 60, 0)], [(168, 0, 0), (150, -36, 0), (120, -60, 0)],
          [(-166, 10, 0), (-240, 30, 0)], [(-166, -10, 0), (-240, -30, 0)], [(-166, 0, 0), (-260, 0, 0)]]
    return L


MODELS = {"carrier": carrier, "su27": su27, "fa18": fa18, "apache": apache, "tank": tank}

# name: (model, azimuth deg (eye bearing from the model, 0 = dead ahead,
# 90 = starboard), elevation deg, field of view deg)
VIEWS = {
    "carrier": ("carrier", 62, 16, 10),
    "carrier-quarter": ("carrier", -140, 20, 12),
    "su27": ("su27", 38, 24, 20),
    "su27-rear": ("su27", 215, 14, 20),
    "fa18": ("fa18", -32, 16, 20),
    "fa18-top": ("fa18", 60, 62, 20),
    "apache": ("apache", 30, 12, 20),
    "tank": ("tank", 36, 18, 20),
}

VIEW_H = 200.0
PAD = 4.0


def project(lines: list[Line], az: float, el: float, fov: float):
    # The models are drawn with y to starboard, which is left-handed with x
    # forward and z up: flip y into a right-handed world, eye included, or
    # every picture comes out mirrored (the carrier's island to port).
    lines = [[(x, -y, z) for (x, y, z) in l] for l in lines]
    pts = np.array([p for l in lines for p in l], dtype=float)
    lo, hi = pts.min(axis=0), pts.max(axis=0)
    target = (lo + hi) / 2
    size = float(np.linalg.norm(hi - lo))
    dist = size / 2 / math.tan(math.radians(fov) / 2)
    a, e = math.radians(az), math.radians(el)
    eye = target + dist * np.array([math.cos(e) * math.cos(a), -math.cos(e) * math.sin(a), math.sin(e)])
    f = target - eye
    f /= np.linalg.norm(f)
    r = np.cross(f, [0, 0, 1.0])
    r /= np.linalg.norm(r)
    u = np.cross(r, f)

    def to2d(p):
        d = np.asarray(p) - eye
        z = float(d @ f)
        return (float(d @ r) / z, -float(d @ u) / z, z)

    proj = [[to2d(p) for p in l] for l in lines]
    xs = [p[0] for l in proj for p in l]
    ys = [p[1] for l in proj for p in l]
    zs = [p[2] for l in proj for p in l]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    scale = (VIEW_H - 2 * PAD) / (y1 - y0)
    width = (x1 - x0) * scale + 2 * PAD
    z_near, z_far = min(zs), max(zs)
    buckets = ["", "", ""]
    for l in proj:
        depth = (sum(p[2] for p in l) / len(l) - z_near) / max(z_far - z_near, 1e-9)
        b = 0 if depth < 0.4 else 1 if depth < 0.7 else 2
        pts2 = [((p[0] - x0) * scale + PAD, (p[1] - y0) * scale + PAD) for p in l]
        seg = "M" + "L".join(f"{x:.1f} {y:.1f}" for (x, y) in pts2)
        buckets[b] += seg
    return width, VIEW_H, buckets


def preview(directory: str):
    """PNGs of every view, for looking at the models without the dashboard."""
    from PIL import Image, ImageDraw
    import re
    d = Path(directory)
    d.mkdir(parents=True, exist_ok=True)
    for name, (model, az, el, fov) in VIEWS.items():
        w, h, paths = project(MODELS[model](), az, el, fov)
        k = 3
        img = Image.new("RGB", (int(w * k), int(h * k)), (12, 16, 10))
        dr = ImageDraw.Draw(img)
        for path, col in zip(paths, ((150, 220, 80), (95, 150, 55), (55, 90, 35))):
            for seg in path.split("M")[1:]:
                pts = [tuple(float(v) * k for v in pt.split()) for pt in seg.split("L")]
                if len(pts) > 1:
                    dr.line(pts, fill=col, width=2)
        img.save(d / f"{name}.png")
    print(f"previews in {d}")


def main():
    import sys
    if len(sys.argv) > 2 and sys.argv[1] == "--preview":
        return preview(sys.argv[2])
    out = [
        "// Generated by scripts/gen-wireframes.py -- edit the models there, not here.",
        "// Per view: the viewBox size and three path strings, near to far.",
        "",
        "export interface WireframeArt {",
        "  w: number",
        "  h: number",
        "  paths: [string, string, string]",
        "}",
        "",
        "export const WIREFRAMES = {",
    ]
    for name, (model, az, el, fov) in VIEWS.items():
        w, h, paths = project(MODELS[model](), az, el, fov)
        out.append(f"  '{name}': {{ w: {w:.0f}, h: {h:.0f}, paths: [")
        for p in paths:
            out.append(f"    '{p}',")
        out.append("  ] },")
        print(f"{name}: {w:.0f}x{h:.0f}, {sum(len(p) for p in paths) // 1024} KiB")
    out += ["} satisfies Record<string, WireframeArt>", "", "export type WireframeName = keyof typeof WIREFRAMES", ""]
    OUT.write_text("\n".join(out), encoding="utf-8", newline="\n")
    print(f"wrote {OUT}")


if __name__ == "__main__":
    main()
