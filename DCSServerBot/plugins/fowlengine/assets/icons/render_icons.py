"""Source of truth for the Fowl Engine Discord icon set.

Discord will only take raster emoji (PNG/GIF, 256KB, square), so the icons
cannot ship as SVG and be uploaded directly. Rather than commit opaque PNGs
with no editable original, the drawing code *is* the original: run this script
and the PNGs next to it are regenerated.

    python render_icons.py

Everything is drawn on a 4x supersampled canvas and box-filtered down, which is
the only way to get clean diagonals out of Pillow's polygon fill. Strokes are a
single weight across the whole set and every glyph sits inside the same optical
margin, so a row of them in an embed field reads as one family instead of a
ransom note.

Install them into Discord with `/feops icons_install` (see icons.py) -- the bot
uploads them as *application* emoji, which work in every guild the app is in
and do not eat the guild's own 50-emoji budget.
"""
import math
import os

from PIL import Image, ImageDraw

SS = 4                      # supersample factor
SIZE = 128                  # final px, square (Discord scales to 32-48 in line)
S = SIZE * SS
PAD = 10 * SS               # optical margin -- keeps glyphs off the emoji edge
W = 9 * SS                  # stroke weight, one value for the whole set

# Vector Strike palette. Kept deliberately small: an icon earns a colour by
# meaning something (severity, coalition, live/dead), never for decoration.
RED = (200, 16, 46, 255)        # brand red / hostile / critical
BLUE = (56, 132, 222, 255)      # friendly coalition
AMBER = (232, 155, 30, 255)     # high urgency / caution
GREEN = (52, 168, 104, 255)     # routine / good / live
STEEL = (150, 162, 176, 255)    # neutral chrome, section headers
WHITE = (238, 242, 247, 255)
DARK = (18, 22, 28, 255)        # detail knocked into a filled glyph
CLEAR = (0, 0, 0, 0)            # a true cut-out: ImageDraw writes, never blends
# Podium only. Gold is the set's amber; silver sits clearly lighter than STEEL
# so a 2nd place never reads as a disabled 1st.
GOLD = AMBER
SILVER = (206, 213, 222, 255)
BRONZE = (186, 116, 62, 255)

OUT_DIR = os.path.dirname(os.path.abspath(__file__))


def _canvas():
    img = Image.new("RGBA", (S, S), (0, 0, 0, 0))
    return img, ImageDraw.Draw(img)


def _save(img: Image.Image, name: str) -> None:
    img.resize((SIZE, SIZE), Image.LANCZOS).save(os.path.join(OUT_DIR, f"{name}.png"))
    print(f"  {name}.png")


def _poly(d, pts, fill):
    d.polygon(pts, fill=fill)


def _line(d, a, b, fill, w=W):
    d.line([a, b], fill=fill, width=w)
    # Pillow does not round line caps; do it by hand so strokes meeting at an
    # angle don't show a notch.
    for (x, y) in (a, b):
        d.ellipse([x - w / 2, y - w / 2, x + w / 2, y + w / 2], fill=fill)


def _ring(d, cx, cy, r, fill, w=W):
    d.ellipse([cx - r, cy - r, cx + r, cy + r], outline=fill, width=w)


def _arc(d, cx, cy, r, start, end, fill, w=W):
    d.arc([cx - r, cy - r, cx + r, cy + r], start, end, fill=fill, width=w)


def _dot(d, cx, cy, r, fill):
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=fill)


def _path(d, pts, fill, w=W):
    """A polyline with round joints -- _line per segment, caps overlapping."""
    for a, b in zip(pts, pts[1:]):
        _line(d, a, b, fill, w)


def _arrowhead(d, tip, ang, size, fill, spread=0.55):
    """Filled head with its point at `tip`, pointing along `ang` (radians,
    screen coordinates: y grows downward)."""
    back = ang + math.pi
    x, y = tip
    _poly(d, [tip,
              (x + size * math.cos(back + spread), y + size * math.sin(back + spread)),
              (x + size * math.cos(back - spread), y + size * math.sin(back - spread))], fill)


def _star(d, cx, cy, r, inner, fill, points=5):
    pts = []
    for i in range(points * 2):
        ang = -math.pi / 2 + math.pi * i / points
        rr = r if i % 2 == 0 else r * inner
        pts.append((cx + rr * math.cos(ang), cy + rr * math.sin(ang)))
    _poly(d, pts, fill)


# ── severity marks ──────────────────────────────────────────────────────────
# Three chevrons, filling upward with urgency. They differ in count and colour
# both, so they stay distinguishable to a colour-blind reader and at 32px.

def _chevrons(count: int, colour):
    # Only the *lit* chevrons are drawn. Ghosting the rest at low alpha looked
    # right on paper and composites to mud against transparency at 32px.
    img, d = _canvas()
    span = S - 2 * PAD
    step = span / max(count, 1) * 0.62
    # Centre the stack vertically whatever the count, so the three icons share
    # an optical baseline when they appear in the same list.
    top = (S - step * (count - 1) - span * 0.34) / 2 + span * 0.17
    for i in range(count):
        y = top + i * step
        _line(d, (PAD + W / 2, y), (S / 2, y - span * 0.30), colour)
        _line(d, (S / 2, y - span * 0.30), (S - PAD - W / 2, y), colour)
    return img


def icon_critical():
    return _chevrons(3, RED)


def icon_high():
    return _chevrons(2, AMBER)


def icon_routine():
    return _chevrons(1, GREEN)


# ── task kinds ──────────────────────────────────────────────────────────────

def icon_defend():
    """Shield."""
    img, d = _canvas()
    top, bot = PAD, S - PAD
    half = (S - 2 * PAD) / 2
    cx = S / 2
    _poly(d, [
        (cx - half, top + half * 0.15), (cx, top),
        (cx + half, top + half * 0.15), (cx + half, top + half * 0.95),
        (cx, bot), (cx - half, top + half * 0.95),
    ], BLUE)
    return img


def icon_capture():
    """Flag on a staff."""
    img, d = _canvas()
    x = PAD + W
    _line(d, (x, PAD), (x, S - PAD), STEEL)
    _poly(d, [
        (x + W / 2, PAD + W / 2), (S - PAD, PAD + (S - 2 * PAD) * 0.22),
        (x + W / 2, PAD + (S - 2 * PAD) * 0.46),
    ], BLUE)
    return img


def icon_strike():
    """Impact burst."""
    img, d = _canvas()
    cx = cy = S / 2
    pts = []
    for i in range(16):
        ang = math.pi * 2 * i / 16
        r = (S / 2 - PAD) * (1.0 if i % 2 == 0 else 0.44)
        pts.append((cx + r * math.cos(ang), cy + r * math.sin(ang)))
    _poly(d, pts, RED)
    return img


def icon_sead():
    """Emitting radar, struck out -- kill the radar, not the airframe."""
    img, d = _canvas()
    cx = S / 2
    base = S - PAD
    # Mast on a tripod.
    _line(d, (cx, S * 0.44), (cx, base), AMBER)
    _line(d, (cx - S * 0.16, base), (cx + S * 0.16, base), AMBER, w=int(W * 0.8))
    # Two emission arcs off the top.
    for r in (S * 0.17, S * 0.29):
        d.arc([cx - r, S * 0.40 - r, cx + r, S * 0.40 + r], 195, 345,
              fill=AMBER, width=int(W * 0.75))
    # The strike. Drawn last and heavier so it reads as the operative mark.
    _line(d, (PAD, S - PAD), (S - PAD, PAD), RED, w=int(W * 1.15))
    return img


def icon_cas():
    """Crosshair over a ground target."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, RED)
    for dx, dy in ((0, -1), (0, 1), (-1, 0), (1, 0)):
        _line(d, (cx + dx * r * 0.45, cy + dy * r * 0.45),
              (cx + dx * (r + W * 0.6), cy + dy * (r + W * 0.6)), RED)
    d.ellipse([cx - W * 0.7, cy - W * 0.7, cx + W * 0.7, cy + W * 0.7], fill=RED)
    return img


def icon_intercept():
    """Delta-wing planform."""
    img, d = _canvas()
    cx = S / 2
    nose, tail = PAD, S - PAD
    half = S / 2 - PAD
    _poly(d, [
        (cx, nose),                                  # nose
        (cx + half * 0.22, S * 0.50),                # fuselage shoulder
        (cx + half, S * 0.74),                       # right wingtip
        (cx + half, S * 0.86),
        (cx + half * 0.20, S * 0.74),                # wing root, trailing edge
        (cx + half * 0.30, tail),                    # right stabiliser
        (cx - half * 0.30, tail),                    # left stabiliser
        (cx - half * 0.20, S * 0.74),
        (cx - half, S * 0.86),
        (cx - half, S * 0.74),                       # left wingtip
        (cx - half * 0.22, S * 0.50),
    ], BLUE)
    return img


def icon_logistics():
    """Supply crate with banding."""
    img, d = _canvas()
    d.rectangle([PAD, PAD + W, S - PAD, S - PAD], outline=AMBER, width=W)
    _line(d, (PAD + W, PAD + W * 2.6), (S - PAD - W, PAD + W * 2.6), AMBER, w=int(W * 0.7))
    _line(d, (S / 2, PAD + W * 2.6), (S / 2, S - PAD - W / 2), AMBER, w=int(W * 0.7))
    return img


def icon_recon():
    """Magnifier -- go and look.

    An eye was tried first and kept collapsing into either a diamond or a
    circle-in-a-circle at emoji size; a glass with a handle is unmistakable at
    32px and carries the same "we have no picture of this" meaning.
    """
    img, d = _canvas()
    cx = cy = S * 0.42
    r = S * 0.30
    # Handle first, so the rim laps over its top end.
    _line(d, (cx + r * 0.68, cy + r * 0.68), (S - PAD, S - PAD), STEEL, w=int(W * 1.1))
    _ring(d, cx, cy, r, STEEL, w=int(W * 0.85))
    return img


def icon_csar():
    """Rotor disc over a medical cross -- pick the pilot up."""
    img, d = _canvas()
    cx = S / 2
    rotor_y = PAD + W * 0.6
    # Rotor: a flat bar with a mast, well clear of the cross below it.
    _line(d, (PAD, rotor_y), (S - PAD, rotor_y), STEEL, w=int(W * 0.7))
    _line(d, (cx, rotor_y), (cx, rotor_y + W * 1.1), STEEL, w=int(W * 0.6))
    # Cross, sized to the space that leaves rather than to the full canvas.
    cy = S * 0.60
    arm, t = (S - 2 * PAD) * 0.33, W * 1.0
    d.rectangle([cx - t, cy - arm, cx + t, cy + arm], fill=GREEN)
    d.rectangle([cx - arm, cy - t, cx + arm, cy + t], fill=GREEN)
    return img


# ── section headers ─────────────────────────────────────────────────────────

def icon_posture():
    """Three bars -- territory balance."""
    img, d = _canvas()
    span = S - 2 * PAD
    bw = span / 4.4
    for i, (h, c) in enumerate(((0.45, BLUE), (0.95, BLUE), (0.65, RED))):
        x = PAD + i * (bw * 1.45)
        d.rectangle([x, S - PAD - span * h, x + bw, S - PAD], fill=c)
    return img


def icon_weather():
    """Cloud, built from lobes on one baseline so it stays symmetric."""
    img, d = _canvas()
    base = S * 0.72                      # flat bottom every lobe sits on
    # (cx frac, r frac, how far the lobe's top rides above the base). The
    # middle lobe is deliberately much bigger -- equal lobes read as a mound.
    lobes = ((0.27, 0.130), (0.48, 0.235), (0.72, 0.160))
    for fx, fr in lobes:
        cx, r = S * fx, S * fr
        d.ellipse([cx - r, base - r * 2, cx + r, base], fill=STEEL)
    left, right = S * 0.27 - S * 0.130, S * 0.72 + S * 0.160
    d.rectangle([left, base - S * 0.13, right, base], fill=STEEL)
    return img


def icon_air():
    """Radar scope with a sweep."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, STEEL, w=int(W * 0.75))
    _ring(d, cx, cy, r * 0.52, STEEL, w=int(W * 0.6))
    d.pieslice([cx - r, cy - r, cx + r, cy + r], -75, -25, fill=(*GREEN[:3], 150))
    _line(d, (cx, cy), (cx + r * math.cos(math.radians(-25)),
                        cy + r * math.sin(math.radians(-25))), GREEN, w=int(W * 0.7))
    return img


def icon_tasking():
    """Clipboard with lines."""
    img, d = _canvas()
    d.rectangle([PAD + W, PAD + W, S - PAD - W, S - PAD], outline=STEEL, width=int(W * 0.8))
    d.rectangle([S * 0.33, PAD, S * 0.67, PAD + W * 1.8], fill=STEEL)
    for i in range(3):
        y = PAD + S * 0.30 + i * S * 0.17
        _line(d, (PAD + W * 2.6, y), (S - PAD - W * 2.2, y), STEEL, w=int(W * 0.6))
    return img


def icon_hotspot():
    """Contested-point marker."""
    img, d = _canvas()
    cx = S / 2
    _poly(d, [(cx, PAD), (S - PAD, S - PAD), (PAD, S - PAD)], AMBER)
    d.rectangle([cx - W * 0.5, S * 0.44, cx + W * 0.5, S * 0.70], fill=DARK)
    d.ellipse([cx - W * 0.5, S * 0.76, cx + W * 0.5, S * 0.76 + W], fill=DARK)
    return img


def icon_threat():
    """SAM ring -- the NATO air-defence idiom, a site inside its envelope."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, RED, w=int(W * 0.7))
    _poly(d, [(cx, cy - r * 0.52), (cx + r * 0.48, cy + r * 0.34),
              (cx - r * 0.48, cy + r * 0.34)], RED)
    return img


def icon_supply():
    """Truck -- the logistics section, distinct from a single crate."""
    img, d = _canvas()
    body_top, body_bot = S * 0.30, S * 0.66
    d.rectangle([PAD, body_top, S * 0.56, body_bot], fill=AMBER)
    _poly(d, [(S * 0.58, body_top + S * 0.10), (S * 0.78, body_top + S * 0.10),
              (S - PAD, body_bot - S * 0.06), (S - PAD, body_bot),
              (S * 0.58, body_bot)], AMBER)
    for x in (S * 0.26, S * 0.80):
        d.ellipse([x - W * 1.25, body_bot - W * 0.5, x + W * 1.25, body_bot + W * 2.0],
                  fill=STEEL)
    return img


def icon_comms():
    """Antenna mast radiating."""
    img, d = _canvas()
    cx = S / 2
    _line(d, (cx, S * 0.34), (cx, S - PAD), STEEL)
    d.ellipse([cx - W * 0.9, S * 0.30 - W * 0.9, cx + W * 0.9, S * 0.30 + W * 0.9], fill=GREEN)
    for r, alpha in ((S * 0.20, 230), (S * 0.33, 140)):
        d.arc([cx - r, S * 0.30 - r, cx + r, S * 0.30 + r], 200, 340,
              fill=(*GREEN[:3], alpha), width=int(W * 0.65))
    return img


def icon_recent():
    """Clock."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, STEEL, w=int(W * 0.8))
    _line(d, (cx, cy), (cx, cy - r * 0.55), STEEL, w=int(W * 0.7))
    _line(d, (cx, cy), (cx + r * 0.42, cy), STEEL, w=int(W * 0.7))
    return img


# ── coalitions and status markers ───────────────────────────────────────────────

def _roundel(colour):
    """A filled disc inside a ring -- reads as a coalition roundel at 32px."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, colour, w=int(W * 0.8))
    d.ellipse([cx - r * 0.5, cy - r * 0.5, cx + r * 0.5, cy + r * 0.5], fill=colour)
    return img


def icon_blue():
    return _roundel(BLUE)


def icon_red():
    return _roundel(RED)


def _status_light(colour):
    """A lit lamp with a halo: live (green) and down (red) are one shape."""
    img, d = _canvas()
    cx = cy = S / 2
    r = (S / 2 - PAD) * 0.62
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=colour)
    _ring(d, cx, cy, r * 1.62, (*colour[:3], 110), w=int(W * 0.6))
    return img


def icon_live():
    return _status_light(GREEN)


def icon_offline():
    img, d = _canvas()
    cx = cy = S / 2
    r = (S / 2 - PAD) * 0.62
    _ring(d, cx, cy, r, STEEL, w=int(W * 0.7))
    return img


def icon_good():
    img, d = _canvas()
    _line(d, (PAD, S * 0.55), (S * 0.42, S - PAD - W), GREEN)
    _line(d, (S * 0.42, S - PAD - W), (S - PAD, PAD + W), GREEN)
    return img


def icon_bad():
    img, d = _canvas()
    _line(d, (PAD, PAD), (S - PAD, S - PAD), RED)
    _line(d, (S - PAD, PAD), (PAD, S - PAD), RED)
    return img


def icon_neutral():
    img, d = _canvas()
    _line(d, (PAD, S / 2), (S - PAD, S / 2), STEEL)
    return img


def icon_link():
    """Arrow out -- the 'open the dashboard' row."""
    img, d = _canvas()
    _line(d, (PAD, S - PAD), (S - PAD - W, PAD + W), BLUE)
    _poly(d, [(S - PAD, PAD), (S - PAD, S * 0.46), (S * 0.54, PAD)], BLUE)
    return img


# ── status and ops notices ──────────────────────────────────────────────────
# The rest of the set: what the ops channel, the admin replies and the status
# embeds need beyond the briefing. Same rules -- colour only where it carries
# the meaning (amber = wait/caution, red = stopped, green = healthy, blue =
# something new arriving), steel for everything that is just a label.

def icon_warning():
    """Outlined caution triangle. The filled one is `hotspot` (a place on the
    map); outline vs fill keeps "this went wrong" apart from "look here"."""
    img, d = _canvas()
    cx = S / 2
    top, bot = PAD + W * 0.6, S - PAD - W * 0.5
    half = (S - 2 * PAD) / 2 - W * 0.5
    _path(d, [(cx, top), (cx + half, bot), (cx - half, bot), (cx, top)], AMBER, w=int(W * 0.85))
    _line(d, (cx, S * 0.42), (cx, S * 0.61), AMBER, w=int(W * 0.9))
    _dot(d, cx, S * 0.73, W * 0.5, AMBER)
    return img


def icon_blocked():
    """No-entry disc: refused, marked bad, impossible."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _dot(d, cx, cy, r, RED)
    d.rectangle([cx - r * 0.62, cy - W * 0.75, cx + r * 0.62, cy + W * 0.75], fill=WHITE)
    return img


def icon_down():
    """Red lamp -- the mirror of `live`: offline, deck closed, range hot."""
    return _status_light(RED)


def icon_info():
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, BLUE, w=int(W * 0.8))
    _dot(d, cx, cy - r * 0.42, W * 0.6, BLUE)
    _line(d, (cx, cy - r * 0.08), (cx, cy + r * 0.52), BLUE, w=int(W * 0.95))
    return img


def icon_alert():
    """Rotating beacon on its base, flashing."""
    img, d = _canvas()
    cx = S / 2
    span = S - 2 * PAD
    base = S - PAD - W * 0.5
    dw, dh = span * 0.24, span * 0.50            # dome half-width and height
    dome_y = base - W * 1.1                      # the dome's flat bottom
    d.pieslice([cx - dw, dome_y - dh, cx + dw, dome_y + dh], 180, 360, fill=RED)
    _line(d, (cx - dw - W, base), (cx + dw + W, base), STEEL)
    # Flash rays, each starting a fixed gap off the dome's curve.
    for deg in (-90, -140, -40, -180, 0):
        a = math.radians(deg)
        edge = dw * dh / math.hypot(dh * math.cos(a), dw * math.sin(a))
        r0, r1 = edge + W * 0.9, edge + W * 2.4
        _line(d, (cx + r0 * math.cos(a), dome_y + r0 * math.sin(a)),
              (cx + r1 * math.cos(a), dome_y + r1 * math.sin(a)), RED, w=int(W * 0.75))
    return img


def icon_pending():
    """Hourglass -- staged, waiting, ready to capture."""
    img, d = _canvas()
    cx = S / 2
    top, bot = PAD + W * 0.5, S - PAD - W * 0.5
    half = (S - 2 * PAD) * 0.30
    cap = half + W * 0.7
    _line(d, (cx - cap, top), (cx + cap, top), AMBER)
    _line(d, (cx - cap, bot), (cx + cap, bot), AMBER)
    g_top, g_bot, mid = top + W * 0.9, bot - W * 0.9, S / 2
    gw = int(W * 0.7)
    _path(d, [(cx - half, g_top), (cx, mid), (cx - half, g_bot)], AMBER, w=gw)
    _path(d, [(cx + half, g_top), (cx, mid), (cx + half, g_bot)], AMBER, w=gw)
    # Sand: a little left above, a pile below.
    t = 0.45
    y = g_top + (mid - g_top) * t
    _poly(d, [(cx - half * (1 - t) * 0.7, y), (cx + half * (1 - t) * 0.7, y), (cx, mid - W * 0.3)], AMBER)
    _poly(d, [(cx, mid + W * 1.3), (cx + half * 0.72, g_bot - W * 0.2), (cx - half * 0.72, g_bot - W * 0.2)], AMBER)
    return img


def icon_paused():
    img, d = _canvas()
    h = (S - 2 * PAD) * 0.36
    for x in (S * 0.36, S * 0.64):
        _line(d, (x, S / 2 - h), (x, S / 2 + h), STEEL, w=int(W * 1.5))
    return img


def icon_restart():
    """Clockwise loop arrow -- restart, rotation, regressed."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD - W * 1.1
    start, end = -35, 215
    _arc(d, cx, cy, r, start, end, STEEL)
    a = math.radians(start)
    _dot(d, cx + r * math.cos(a), cy + r * math.sin(a), W / 2, STEEL)
    e = math.radians(end)
    ex, ey = cx + r * math.cos(e), cy + r * math.sin(e)
    tangent = math.atan2(math.cos(e), -math.sin(e))     # clockwise direction of travel
    size = W * 3.0
    _arrowhead(d, (ex + math.cos(tangent) * size * 0.75, ey + math.sin(tangent) * size * 0.75),
               tangent, size, STEEL, spread=0.66)
    return img


def _double_triangle(point_left: bool, colour):
    img, d = _canvas()
    span = S - 2 * PAD
    cy, h, w = S / 2, span * 0.60, span / 2
    for i in range(2):
        xa = PAD + i * w
        xb = xa + w
        if point_left:
            _poly(d, [(xa, cy), (xb, cy - h / 2), (xb, cy + h / 2)], colour)
        else:
            _poly(d, [(xb, cy), (xa, cy - h / 2), (xa, cy + h / 2)], colour)
    return img


def icon_rollback():
    """Rewind -- amber, because a rollback means something already went wrong."""
    return _double_triangle(True, AMBER)


def icon_forward():
    """Fast-forward -- "more than this, elsewhere"."""
    return _double_triangle(False, STEEL)


def icon_update():
    """Arrow down into a tray -- a new build arriving."""
    img, d = _canvas()
    cx = S / 2
    tray_y = S - PAD - W * 0.5
    _path(d, [(PAD + W * 0.5, S * 0.62), (PAD + W * 0.5, tray_y),
              (S - PAD - W * 0.5, tray_y), (S - PAD - W * 0.5, S * 0.62)], STEEL)
    tip = S * 0.70
    _line(d, (cx, PAD), (cx, tip - W * 1.6), BLUE, w=int(W * 1.1))
    _arrowhead(d, (cx, tip), math.pi / 2, W * 3.0, BLUE, spread=0.72)
    return img


def icon_build():
    """Puzzle piece -- one engine component (bflib, bfdb, bftools)."""
    img, d = _canvas()
    span = S - 2 * PAD
    k = span * 0.14                        # knob radius
    x0, y0 = PAD, PAD + k * 1.6
    x1, y1 = S - PAD - k * 1.6, S - PAD
    d.rectangle([x0, y0, x1, y1], fill=STEEL)
    _dot(d, (x0 + x1) / 2, y0 - k * 0.55, k, STEEL)          # knob on top
    _dot(d, x1 + k * 0.55, (y0 + y1) / 2, k, STEEL)          # knob on the right
    _dot(d, x0 + k * 0.45, (y0 + y1) / 2, k * 0.85, CLEAR)    # socket on the left
    return img


def icon_probation():
    """Flask with liquid -- a new build under test."""
    img, d = _canvas()
    cx = S / 2
    neck_w = (S - 2 * PAD) * 0.13
    neck_top, shoulder = PAD + W * 0.5, S * 0.40
    base = S - PAD - W * 0.5
    half = (S - 2 * PAD) * 0.40
    # Liquid first so the glass outline sits over it.
    fill_y = S * 0.62
    t = (fill_y - shoulder) / (base - shoulder)
    _poly(d, [(cx - neck_w - (half - neck_w) * t, fill_y), (cx + neck_w + (half - neck_w) * t, fill_y),
              (cx + half, base), (cx - half, base)], AMBER)
    gw = int(W * 0.75)
    _path(d, [(cx - neck_w, neck_top), (cx - neck_w, shoulder), (cx - half, base),
              (cx + half, base), (cx + neck_w, shoulder), (cx + neck_w, neck_top)], STEEL, w=gw)
    _line(d, (cx - neck_w - W * 0.6, neck_top), (cx + neck_w + W * 0.6, neck_top), STEEL, w=gw)
    return img


def icon_new():
    """Four-point sparkle, with a small one -- a first-seen issue."""
    img, d = _canvas()
    _star(d, S * 0.44, S * 0.56, (S - 2 * PAD) * 0.44, 0.28, GREEN, points=4)
    _star(d, S * 0.78, S * 0.24, (S - 2 * PAD) * 0.17, 0.32, GREEN, points=4)
    return img


def icon_priority():
    img, d = _canvas()
    _star(d, S / 2, S / 2 + (S - 2 * PAD) * 0.05, S / 2 - PAD, 0.46, AMBER)
    return img


def icon_campaign():
    """Folder -- a campaign pack (cfg + mission files)."""
    img, d = _canvas()
    top, bot = PAD + W * 0.8, S - PAD - W * 0.4
    tab_w = (S - 2 * PAD) * 0.40
    _poly(d, [(PAD, top), (PAD + tab_w, top), (PAD + tab_w + W * 1.2, top + W * 1.2),
              (S - PAD, top + W * 1.2), (S - PAD, bot), (PAD, bot)], STEEL)
    # Front flap edge, knocked out, so it reads as a folder and not a tag.
    d.line([(PAD, top + W * 2.7), (S - PAD, top + W * 2.7)], fill=CLEAR, width=int(W * 0.45))
    return img


def icon_settings():
    """Gear."""
    img, d = _canvas()
    cx = cy = S / 2
    r_out, r_in = S / 2 - PAD, (S / 2 - PAD) * 0.74
    teeth = 8
    pts = []
    for i in range(teeth * 4):
        ang = 2 * math.pi * (i - 0.5) / (teeth * 4)
        rr = r_out if (i % 4) in (1, 2) else r_in
        pts.append((cx + rr * math.cos(ang), cy + rr * math.sin(ang)))
    _poly(d, pts, STEEL)
    _dot(d, cx, cy, r_in * 0.42, CLEAR)
    return img


# ── hardware (the perf embed) ───────────────────────────────────────────────

def icon_cpu():
    """Chip with pins -- CPU, and the GPU row too."""
    img, d = _canvas()
    cx = cy = S / 2
    body = (S - 2 * PAD) * 0.30
    pin = W * 1.3
    pw = int(W * 0.6)
    d.rectangle([cx - body, cy - body, cx + body, cy + body], fill=STEEL)
    d.rectangle([cx - body * 0.45, cy - body * 0.45, cx + body * 0.45, cy + body * 0.45], fill=CLEAR)
    for f in (-0.55, 0, 0.55):
        o = body * f
        _line(d, (cx + o, cy - body), (cx + o, cy - body - pin), STEEL, w=pw)
        _line(d, (cx + o, cy + body), (cx + o, cy + body + pin), STEEL, w=pw)
        _line(d, (cx - body, cy + o), (cx - body - pin, cy + o), STEEL, w=pw)
        _line(d, (cx + body, cy + o), (cx + body + pin, cy + o), STEEL, w=pw)
    return img


def icon_memory():
    """RAM stick: chips on a board, contacts along the edge."""
    img, d = _canvas()
    top, bot = S * 0.30, S * 0.66
    d.rectangle([PAD, top, S - PAD, bot], fill=STEEL)
    span = S - 2 * PAD
    cw = span * 0.20
    for i in range(3):
        x = PAD + span * 0.085 + i * (cw + span * 0.09)
        d.rectangle([x, top + W * 0.7, x + cw, top + W * 2.4], fill=CLEAR)
    # Contacts: teeth below the board, with the keying notch at the centre.
    for i in range(7):
        x = PAD + W * 0.8 + i * (span - W * 1.6) / 6
        if i == 3:
            continue
        _line(d, (x, bot), (x, bot + W * 1.4), STEEL, w=int(W * 0.55))
    return img


def icon_disk():
    """Floppy -- storage."""
    img, d = _canvas()
    lo, hi = PAD + W * 0.2, S - PAD - W * 0.2
    cut = (hi - lo) * 0.16
    _poly(d, [(lo, lo), (hi - cut, lo), (hi, lo + cut), (hi, hi), (lo, hi)], STEEL)
    span = hi - lo
    d.rectangle([lo + span * 0.24, lo, lo + span * 0.70, lo + span * 0.30], fill=CLEAR)
    d.rectangle([lo + span * 0.54, lo + span * 0.05, lo + span * 0.63, lo + span * 0.24], fill=STEEL)
    d.rectangle([lo + span * 0.16, lo + span * 0.52, hi - span * 0.16, hi - span * 0.06], fill=CLEAR)
    return img


def icon_temp():
    """Thermometer, red column."""
    img, d = _canvas()
    cx = S / 2
    bulb_r = (S - 2 * PAD) * 0.17
    bulb_y = S - PAD - bulb_r
    tube = W * 1.25
    top = PAD + tube
    gw = int(W * 0.7)
    # Glass: the tube's walls and rounded top, then the bulb ring.
    _arc(d, cx, top, tube, 180, 360, STEEL, w=gw)
    _line(d, (cx - tube, top), (cx - tube, bulb_y - bulb_r * 0.8), STEEL, w=gw)
    _line(d, (cx + tube, top), (cx + tube, bulb_y - bulb_r * 0.8), STEEL, w=gw)
    _ring(d, cx, bulb_y, bulb_r + W * 0.35, STEEL, w=gw)
    _dot(d, cx, bulb_y - bulb_r * 0.9, tube - W * 0.25, CLEAR)   # open the ring into the tube
    _dot(d, cx, bulb_y, bulb_r - W * 0.15, RED)
    _line(d, (cx, S * 0.42), (cx, bulb_y), RED, w=int(W * 0.8))
    for y in (S * 0.30, S * 0.42, S * 0.54):
        _line(d, (cx + tube + W * 0.9, y), (cx + tube + W * 1.9, y), STEEL, w=int(W * 0.45))
    return img


def icon_perf():
    """Pulse trace -- frame time, mission health."""
    img, d = _canvas()
    y = S * 0.56
    _path(d, [(PAD, y), (S * 0.30, y), (S * 0.40, S * 0.24), (S * 0.53, S * 0.80),
              (S * 0.63, S * 0.44), (S * 0.71, y), (S - PAD, y)], GREEN, w=int(W * 0.85))
    return img


def icon_server():
    """Two rack units with a lamp -- the DCS server."""
    img, d = _canvas()
    lo, hi = PAD, S - PAD
    gap = W * 0.9
    h = (hi - lo - gap) / 2
    for i in range(2):
        y0 = lo + i * (h + gap) + W * 0.1
        d.rounded_rectangle([lo, y0, hi, y0 + h - W * 0.2], radius=W * 0.8, outline=STEEL,
                            width=int(W * 0.75))
        cy = y0 + (h - W * 0.2) / 2
        _dot(d, lo + W * 2.0, cy, W * 0.55, GREEN)
        _line(d, (S * 0.52, cy), (hi - W * 1.8, cy), STEEL, w=int(W * 0.55))
    return img


def icon_connect():
    """Plug with its lead -- the connect details."""
    img, d = _canvas()
    cx = S / 2
    body_top, body_bot = S * 0.30, S * 0.60
    bw = (S - 2 * PAD) * 0.28
    for dx in (-bw * 0.45, bw * 0.45):
        _line(d, (cx + dx, PAD + W * 0.2), (cx + dx, body_top), STEEL, w=int(W * 0.75))
    d.rounded_rectangle([cx - bw, body_top, cx + bw, body_bot], radius=W * 0.9, fill=STEEL)
    _poly(d, [(cx - bw * 0.72, body_bot - 1), (cx + bw * 0.72, body_bot - 1),
              (cx + bw * 0.28, body_bot + W * 1.3), (cx - bw * 0.28, body_bot + W * 1.3)], STEEL)
    _path(d, [(cx, body_bot + W * 1.2), (cx, S * 0.80), (cx + bw * 0.9, S - PAD - W * 0.3)],
          STEEL, w=int(W * 0.7))
    return img


# ── objectives, results and people ──────────────────────────────────────────

def icon_captured():
    """Trophy -- an objective taken."""
    img, d = _canvas()
    cx = S / 2
    span = S - 2 * PAD
    bw = span * 0.27                      # bowl half-width
    top = PAD + W * 0.2
    rim = top + span * 0.10
    d.rectangle([cx - bw, top, cx + bw, rim], fill=GOLD)
    d.pieslice([cx - bw, rim - bw, cx + bw, rim + bw], 0, 180, fill=GOLD)
    hr = span * 0.13
    _arc(d, cx - bw, top + hr, hr, 90, 270, GOLD, w=int(W * 0.7))
    _arc(d, cx + bw, top + hr, hr, 270, 450, GOLD, w=int(W * 0.7))
    base_top = S - PAD - W * 1.3
    d.rectangle([cx - W * 0.5, rim + bw - W, cx + W * 0.5, base_top], fill=GOLD)
    d.rectangle([cx - bw * 0.62, base_top - W * 0.9, cx + bw * 0.62, base_top], fill=GOLD)
    d.rectangle([cx - bw * 0.85, base_top, cx + bw * 0.85, S - PAD], fill=GOLD)
    return img


def icon_neutralised():
    """White flag -- an objective knocked to neutral."""
    img, d = _canvas()
    x = PAD + W
    _line(d, (x, PAD), (x, S - PAD), STEEL)
    fx0, fy0 = x + W * 0.5, PAD + W * 0.6
    fx1, fy1 = S - PAD - W * 0.4, PAD + (S - 2 * PAD) * 0.50
    d.rectangle([fx0, fy0, fx1, fy1], fill=WHITE, outline=STEEL, width=int(W * 0.55))
    return img


def icon_unowned():
    """Steel roundel -- neutral ground, beside the Blue and Red ones."""
    return _roundel(STEEL)


def _medal(colour):
    img, d = _canvas()
    cx = S / 2
    span = S - 2 * PAD
    disc_r = span * 0.27
    disc_y = S - PAD - disc_r
    # Ribbon: two straps meeting behind the disc.
    for sgn in (-1, 1):
        _poly(d, [(cx + sgn * span * 0.34, PAD), (cx + sgn * span * 0.10, PAD),
                  (cx - sgn * span * 0.06, disc_y - disc_r * 0.3),
                  (cx + sgn * span * 0.16, disc_y - disc_r * 0.3)], STEEL)
    _dot(d, cx, disc_y, disc_r + W * 0.35, CLEAR)
    _dot(d, cx, disc_y, disc_r, colour)
    _ring(d, cx, disc_y, disc_r * 0.62, CLEAR, w=int(W * 0.35))
    return img


def icon_gold():
    return _medal(GOLD)


def icon_silver():
    return _medal(SILVER)


def icon_bronze():
    return _medal(BRONZE)


def icon_players():
    """Two figures -- players online."""
    img, d = _canvas()
    hr = (S - 2 * PAD) * 0.16

    def figure(cx, top, scale, fill, grow=0.0):
        r = hr * scale
        _dot(d, cx, top + r, r + grow, fill)
        sw = r * 1.85
        d.pieslice([cx - sw - grow, top + r * 2.5 - grow, cx + sw + grow, top + r * 2.5 + sw * 2 + grow],
                   180, 360, fill=fill)

    figure(S * 0.66, PAD + W * 0.2, 0.86, STEEL)
    # Knock a gap around the front figure so the two don't merge at 32px.
    figure(S * 0.40, PAD + W * 1.8, 1.0, CLEAR, grow=W * 0.5)
    figure(S * 0.40, PAD + W * 1.8, 1.0, STEEL)
    return img


# ── range feed: conditions and assets ───────────────────────────────────────

def icon_day():
    img, d = _canvas()
    cx = cy = S / 2
    r = (S / 2 - PAD) * 0.46
    _dot(d, cx, cy, r, AMBER)
    for i in range(8):
        a = math.pi * 2 * i / 8
        r0, r1 = r + W * 0.9, S / 2 - PAD - W * 0.3
        _line(d, (cx + r0 * math.cos(a), cy + r0 * math.sin(a)),
              (cx + r1 * math.cos(a), cy + r1 * math.sin(a)), AMBER, w=int(W * 0.7))
    return img


def icon_night():
    """Crescent: a disc with an offset disc cut out of it."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _dot(d, cx, cy, r, STEEL)
    _dot(d, cx + r * 0.52, cy - r * 0.34, r * 0.86, CLEAR)
    return img


def icon_wind():
    """Streamlines with curled ends."""
    img, d = _canvas()
    gw = int(W * 0.75)
    for y, x1, cr in ((S * 0.33, S * 0.66, S * 0.09), (S * 0.53, S * 0.78, S * 0.10),
                      (S * 0.73, S * 0.56, S * 0.08)):
        _line(d, (PAD, y), (x1, y), STEEL, w=gw)
        # the curl: an arc up from the line's end, back over the top
        _arc(d, x1, y - cr, cr, 270, 450, STEEL, w=gw)
    return img


def icon_fuel():
    """Fuel drop -- tankers on station."""
    img, d = _canvas()
    cx = S / 2
    r = (S - 2 * PAD) * 0.30
    cy = S - PAD - r
    _dot(d, cx, cy, r, AMBER)
    ang = math.radians(40)
    _poly(d, [(cx, PAD), (cx + r * math.cos(ang), cy - r * math.sin(ang)),
              (cx - r * math.cos(ang), cy - r * math.sin(ang))], AMBER)
    return img


def icon_carrier():
    """Anchor -- the carriers."""
    img, d = _canvas()
    cx = S / 2
    ring_r = W * 1.05
    ring_y = PAD + ring_r
    gw = int(W * 0.8)
    _ring(d, cx, ring_y, ring_r, STEEL, w=gw)
    shank_bot = S - PAD - W * 0.5
    _line(d, (cx, ring_y + ring_r), (cx, shank_bot), STEEL, w=gw)
    stock = (S - 2 * PAD) * 0.22
    _line(d, (cx - stock, ring_y + ring_r + W * 1.1), (cx + stock, ring_y + ring_r + W * 1.1), STEEL, w=gw)
    arm_r = (S - 2 * PAD) * 0.38
    arm_cy = shank_bot - arm_r
    _arc(d, cx, arm_cy, arm_r, 20, 160, STEEL, w=gw)
    for deg, sgn in ((20, 1), (160, -1)):
        a = math.radians(deg)
        tip = (cx + arm_r * math.cos(a), arm_cy + arm_r * math.sin(a))
        _arrowhead(d, (tip[0] + sgn * W * 0.2, tip[1] - W * 1.4), -math.pi / 2, W * 1.9, STEEL, spread=0.7)
    return img


# ── the community plugins ───────────────────────────────────────────────────
# What about, faq, rules, tickets, smartmod and radio post (they reach this set
# through their own _icons.py). Same rules again: steel for plain labels,
# green = go, red = stop, blue = something arriving or a pointer elsewhere,
# amber = waiting on someone.

def icon_play():
    """Play -- green, the go of the transport controls (resume, play/pause)."""
    img, d = _canvas()
    span = S - 2 * PAD
    h = span * 0.80
    w = h * 0.87
    x0 = S / 2 - w * 0.40                 # centroid, not bounding box, on the centre
    _poly(d, [(x0, S / 2 - h / 2), (x0 + w, S / 2), (x0, S / 2 + h / 2)], GREEN)
    return img


def icon_skip():
    """Skip to the next track: play head against a bar."""
    img, d = _canvas()
    span = S - 2 * PAD
    h = span * 0.66
    w = h * 0.87
    bar = W * 1.3
    x0 = S / 2 - (w + bar * 1.6) / 2
    _poly(d, [(x0, S / 2 - h / 2), (x0 + w, S / 2), (x0, S / 2 + h / 2)], STEEL)
    xb = x0 + w + bar * 0.6
    d.rectangle([xb, S / 2 - h / 2, xb + bar, S / 2 + h / 2], fill=STEEL)
    return img


def icon_stop():
    """Stop -- a red square: the transmission is cut."""
    img, d = _canvas()
    half = (S - 2 * PAD) * 0.36
    d.rounded_rectangle([S / 2 - half, S / 2 - half, S / 2 + half, S / 2 + half],
                        radius=W * 0.7, fill=RED)
    return img


def _chevron(point_left: bool):
    img, d = _canvas()
    h = (S - 2 * PAD) * 0.36
    w = h * 0.78
    tip, back = (S / 2 - w / 2, S / 2 + w / 2) if point_left else (S / 2 + w / 2, S / 2 - w / 2)
    _path(d, [(back, S / 2 - h), (tip, S / 2), (back, S / 2 + h)], STEEL, w=int(W * 1.1))
    return img


def icon_prev():
    """Page back."""
    return _chevron(True)


def icon_next():
    """Page on."""
    return _chevron(False)


def icon_queue():
    """Playlist: three lines and a play head -- what's coming up."""
    img, d = _canvas()
    gw = int(W * 0.8)
    for y, x1 in ((S * 0.28, S * 0.78), (S * 0.46, S * 0.78), (S * 0.64, S * 0.48)):
        _line(d, (PAD + W * 0.4, y), (x1, y), STEEL, w=gw)
    h = (S - 2 * PAD) * 0.30
    x0 = S * 0.60
    _poly(d, [(x0, S * 0.60), (x0 + h * 0.9, S * 0.60 + h / 2), (x0, S * 0.60 + h)], STEEL)
    return img


def icon_music():
    """Two beamed quavers."""
    img, d = _canvas()
    span = S - 2 * PAD
    hr = span * 0.14                      # note-head radius
    heads = ((S * 0.30, S - PAD - hr), (S * 0.72, S - PAD - hr - span * 0.10))
    beam_t = W * 1.5
    tops = (PAD + span * 0.14, PAD + span * 0.04)
    sw = int(W * 0.8)
    for (hx, hy), top in zip(heads, tops):
        _dot(d, hx, hy, hr, STEEL)
        x = hx + hr - sw / 2
        d.rectangle([x - sw / 2, top, x + sw / 2, hy], fill=STEEL)
    (x_a, _), (x_b, _) = heads
    xa, xb = x_a + hr - sw, x_b + hr
    _poly(d, [(xa, tops[0]), (xb, tops[1]), (xb, tops[1] + beam_t), (xa, tops[0] + beam_t)], STEEL)
    return img


def icon_volume():
    """Speaker with two waves."""
    img, d = _canvas()
    span = S - 2 * PAD
    cy = S / 2
    left = PAD + span * 0.07                  # the waves are shorter than the box: centre the whole
    bx0, bx1 = left, left + span * 0.20
    bh = span * 0.14
    d.rectangle([bx0, cy - bh, bx1, cy + bh], fill=STEEL)
    cone_x = left + span * 0.46
    _poly(d, [(bx1 - 1, cy - bh), (cone_x, cy - span * 0.36), (cone_x, cy + span * 0.36),
              (bx1 - 1, cy + bh)], STEEL)
    ax = cone_x - span * 0.06
    for r in (span * 0.26, span * 0.44):
        _arc(d, ax, cy, r, -48, 48, STEEL, w=int(W * 0.75))
    return img


def icon_shuffle():
    """Two crossing arrows, one passing under the other."""
    img, d = _canvas()
    gw = int(W * 0.8)
    top, bot = S * 0.30, S * 0.70
    x_in, x_a, x_b = PAD + gw * 0.5, S * 0.32, S * 0.62
    head = W * 2.4
    x_end = S - PAD - head * 0.8
    under = [(x_in, top), (x_a, top), (x_b, bot), (x_end, bot)]
    over = [(x_in, bot), (x_a, bot), (x_b, top), (x_end, top)]
    _path(d, under, STEEL, w=gw)
    # Knock a gap where the second arrow crosses, so it reads as over/under.
    _path(d, over[1:3], CLEAR, w=int(gw * 2.6))
    _path(d, over, STEEL, w=gw)
    for y in (top, bot):
        _arrowhead(d, (S - PAD, y), 0, head, STEEL, spread=0.62)
    return img


def icon_filter():
    """Three mixer faders -- tune the output."""
    img, d = _canvas()
    knob = W * 1.35
    top, bot = PAD + knob * 0.4, S - PAD - knob * 0.4
    for x, ky in ((S * 0.27, S * 0.64), (S * 0.50, S * 0.34), (S * 0.73, S * 0.56)):
        _line(d, (x, top), (x, bot), STEEL, w=int(W * 0.55))
        d.rounded_rectangle([x - knob, ky - knob * 0.62, x + knob, ky + knob * 0.62],
                            radius=W * 0.4, fill=STEEL)
    return img


def icon_globe():
    """Globe with a meridian and parallels -- the website, a country."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    gw = int(W * 0.62)
    _ring(d, cx, cy, r, BLUE, w=int(W * 0.8))
    d.ellipse([cx - r * 0.42, cy - r, cx + r * 0.42, cy + r], outline=BLUE, width=gw)
    _line(d, (cx - r + gw * 0.5, cy), (cx + r - gw * 0.5, cy), BLUE, w=gw)
    for f in (-0.52, 0.52):
        half = r * math.sqrt(1 - f * f) * 0.92
        _line(d, (cx - half, cy + r * f), (cx + half, cy + r * f), BLUE, w=gw)
    return img


def icon_ticket():
    """Ticket stub: notched sides and a perforation -- a support request,
    amber because it is waiting on someone."""
    img, d = _canvas()
    top, bot = S * 0.26, S * 0.74
    d.rounded_rectangle([PAD, top, S - PAD, bot], radius=W * 0.6, fill=AMBER)
    notch = (bot - top) * 0.20
    for x in (PAD, S - PAD):
        _dot(d, x, S / 2, notch, CLEAR)
    xp = S * 0.64
    n = 5
    for i in range(n):
        y = top + (bot - top) * (i + 0.5) / n
        _dot(d, xp, y, W * 0.36, CLEAR)
    return img


def icon_lock():
    """Padlock -- closed, locked."""
    img, d = _canvas()
    cx = S / 2
    span = S - 2 * PAD
    bw = span * 0.34
    body_top = S * 0.46
    d.rounded_rectangle([cx - bw, body_top, cx + bw, S - PAD], radius=W * 0.7, fill=STEEL)
    sr = bw * 0.66                            # shackle radius
    sy = PAD + sr + W * 0.4
    gw = int(W * 0.95)
    _arc(d, cx, sy, sr, 180, 360, STEEL, w=gw)
    for x in (cx - sr + gw / 2, cx + sr - gw / 2):
        d.rectangle([x - gw / 2, sy, x + gw / 2, body_top + 1], fill=STEEL)
    _dot(d, cx, body_top + (S - PAD - body_top) * 0.40, W * 0.75, CLEAR)
    d.rectangle([cx - W * 0.3, body_top + (S - PAD - body_top) * 0.40,
                 cx + W * 0.3, body_top + (S - PAD - body_top) * 0.72], fill=CLEAR)
    return img


def icon_mail():
    """Envelope -- open a ticket, something arriving in the inbox."""
    img, d = _canvas()
    top, bot = S * 0.24, S * 0.76
    lo, hi = PAD + W * 0.2, S - PAD - W * 0.2
    gw = int(W * 0.8)
    d.rounded_rectangle([lo, top, hi, bot], radius=W * 0.6, outline=BLUE, width=gw)
    _path(d, [(lo + gw * 0.6, top + gw * 0.6), (S / 2, S * 0.54), (hi - gw * 0.6, top + gw * 0.6)],
          BLUE, w=gw)
    return img


def icon_help():
    """Question mark in a ring -- the sibling of `info`."""
    img, d = _canvas()
    cx = cy = S / 2
    r = S / 2 - PAD
    _ring(d, cx, cy, r, BLUE, w=int(W * 0.8))
    qr = r * 0.28
    qy = cy - r * 0.20
    gw = int(W * 0.9)
    _arc(d, cx, qy, qr, 190, 440, BLUE, w=gw)
    a = math.radians(440)
    _line(d, (cx + qr * math.cos(a), qy + qr * math.sin(a) - gw * 0.3), (cx, cy + r * 0.20), BLUE, w=gw)
    _dot(d, cx, cy + r * 0.50, W * 0.6, BLUE)
    return img


def icon_rules():
    """Scroll, rolled at both ends."""
    img, d = _canvas()
    span = S - 2 * PAD
    body_l, body_r = S / 2 - span * 0.32, S / 2 + span * 0.32
    roll = W * 1.1                            # half-height of a roll
    top_y, bot_y = PAD + roll, S - PAD - roll
    d.rectangle([body_l, top_y, body_r, bot_y], fill=STEEL)
    for y in (top_y, bot_y):
        d.rounded_rectangle([body_l - W * 1.2, y - roll, body_r + W * 1.2, y + roll],
                            radius=roll, fill=STEEL)
        _line(d, (body_l - W * 0.2, y), (body_r + W * 0.2, y), CLEAR, w=int(W * 0.3))
    for i in range(4):
        y = top_y + roll + W * 1.2 + i * (bot_y - top_y - 2 * roll - W * 2.4) / 3
        x1 = body_r - W * 1.3 - (span * 0.16 if i == 3 else 0)
        _line(d, (body_l + W * 1.3, y), (x1, y), CLEAR, w=int(W * 0.55))
    return img


def icon_rocket():
    """Rocket climbing to the right -- start here, get going."""
    # Drawn upright on a double-size canvas and turned 45 degrees: along the
    # diagonal the glyph can run past the square margin it has to fit in once
    # it is turned, which is what keeps it from looking undersized.
    big = Image.new("RGBA", (S * 2, S * 2), (0, 0, 0, 0))
    d = ImageDraw.Draw(big)
    c = S                                     # centre of the big canvas
    L = S / 2 - PAD
    bw = L * 0.25                             # body half-width

    def at(w, a):                             # (radial, axial) about the centre; a < 0 is the nose
        return (c + w, c + a)

    nose, shoulder, tail = -1.22 * L, -0.50 * L, 0.52 * L
    # Exhaust first, so the body laps over its top.
    _poly(d, [at(-bw * 0.62, tail - W * 0.4), at(bw * 0.62, tail - W * 0.4), at(0, 1.12 * L)], AMBER)
    for sgn in (-1, 1):
        _poly(d, [at(sgn * bw, 0.02 * L), at(sgn * bw * 2.15, 0.46 * L),
                  at(sgn * bw * 2.15, 0.70 * L), at(sgn * bw, tail)], STEEL)
    _poly(d, [at(0, nose), at(bw * 0.78, nose + (shoulder - nose) * 0.45), at(bw, shoulder),
              at(bw, tail), at(-bw, tail), at(-bw, shoulder),
              at(-bw * 0.78, nose + (shoulder - nose) * 0.45)], STEEL)
    _dot(d, c, c - 0.26 * L, bw * 0.46, CLEAR)
    big = big.rotate(-45, resample=Image.BICUBIC, center=(c, c))
    return big.crop((S // 2, S // 2, S // 2 + S, S // 2 + S))


def icon_wings():
    """Aviator's wings -- the campaign itself, and its pilots."""
    img, d = _canvas()
    cx = S / 2
    span = S - 2 * PAD
    cy = S * 0.53
    hub = span * 0.13
    t = W * 0.55                              # feather half-thickness
    for sgn in (-1, 1):
        for i, (reach, lift) in enumerate(((0.50, 0.16), (0.40, 0.10), (0.29, 0.05))):
            y = cy - span * 0.12 + i * span * 0.13
            x_in = cx + sgn * hub * 0.6
            x_out = cx + sgn * span * reach
            _poly(d, [(x_in, y - t), (x_out, y - t - span * lift),
                      (x_out - sgn * t * 1.6, y + t - span * lift * 0.5), (x_in, y + t * 1.2)], GOLD)
    _dot(d, cx, cy, hub + W * 0.35, CLEAR)
    _dot(d, cx, cy, hub, GOLD)
    _star(d, cx, cy + hub * 0.05, hub * 0.72, 0.45, DARK)
    return img


def icon_keyboard():
    """Keyboard -- chat commands."""
    img, d = _canvas()
    top, bot = S * 0.28, S * 0.72
    d.rounded_rectangle([PAD, top, S - PAD, bot], radius=W * 0.8, fill=STEEL)
    k = W * 0.95                              # key size
    inner_l, inner_r = PAD + W * 1.1, S - PAD - W * 1.1
    for row, y in enumerate((top + W * 1.1, top + W * 1.1 + k * 1.55)):
        n = 6 if row == 0 else 5
        step = (inner_r - inner_l - k) / (n - 1)
        off = 0 if row == 0 else step / 2
        for i in range(n):
            x = inner_l + off + i * step
            if row == 1 and x + k > inner_r:
                continue
            d.rectangle([x, y, x + k, y + k], fill=CLEAR)
    sy = bot - W * 1.1 - k
    d.rectangle([S / 2 - (S - 2 * PAD) * 0.24, sy, S / 2 + (S - 2 * PAD) * 0.24, sy + k], fill=CLEAR)
    return img


def icon_book():
    """Open book -- the wiki."""
    img, d = _canvas()
    cx = S / 2
    gap = W * 0.45
    top, bot = S * 0.26, S * 0.76
    dip = S * 0.05
    for sgn in (-1, 1):
        outer = cx + sgn * (S / 2 - PAD)
        inner = cx + sgn * gap
        _poly(d, [(inner, top + dip), (outer, top), (outer, bot - dip), (inner, bot)], STEEL)
        for i in range(3):
            f = 0.26 + i * 0.20
            y_in, y_out = top + dip + (bot - top - dip) * f, top + (bot - top - dip) * f
            a = (inner + sgn * W * 1.1, y_in)
            b = (outer - sgn * W * 1.1, y_out)
            _line(d, a, b, CLEAR, w=int(W * 0.4))
    return img


# Emoji name -> drawing function. The names are what icons.py looks up in
# Discord, so they are part of the contract: renaming one orphans the uploaded
# emoji and the briefing silently falls back to its unicode stand-in.
ICONS = {
    "vs_critical": icon_critical,
    "vs_high": icon_high,
    "vs_routine": icon_routine,
    "vs_defend": icon_defend,
    "vs_capture": icon_capture,
    "vs_strike": icon_strike,
    "vs_sead": icon_sead,
    "vs_cas": icon_cas,
    "vs_intercept": icon_intercept,
    "vs_logistics": icon_logistics,
    "vs_recon": icon_recon,
    "vs_csar": icon_csar,
    "vs_posture": icon_posture,
    "vs_weather": icon_weather,
    "vs_air": icon_air,
    "vs_tasking": icon_tasking,
    "vs_hotspot": icon_hotspot,
    "vs_threat": icon_threat,
    "vs_supply": icon_supply,
    "vs_comms": icon_comms,
    "vs_recent": icon_recent,
    "vs_blue": icon_blue,
    "vs_red": icon_red,
    "vs_live": icon_live,
    "vs_offline": icon_offline,
    "vs_good": icon_good,
    "vs_bad": icon_bad,
    "vs_neutral": icon_neutral,
    "vs_link": icon_link,
    "vs_warning": icon_warning,
    "vs_blocked": icon_blocked,
    "vs_down": icon_down,
    "vs_info": icon_info,
    "vs_alert": icon_alert,
    "vs_pending": icon_pending,
    "vs_paused": icon_paused,
    "vs_restart": icon_restart,
    "vs_rollback": icon_rollback,
    "vs_forward": icon_forward,
    "vs_update": icon_update,
    "vs_build": icon_build,
    "vs_probation": icon_probation,
    "vs_new": icon_new,
    "vs_priority": icon_priority,
    "vs_campaign": icon_campaign,
    "vs_settings": icon_settings,
    "vs_cpu": icon_cpu,
    "vs_memory": icon_memory,
    "vs_disk": icon_disk,
    "vs_temp": icon_temp,
    "vs_perf": icon_perf,
    "vs_server": icon_server,
    "vs_connect": icon_connect,
    "vs_captured": icon_captured,
    "vs_neutralised": icon_neutralised,
    "vs_unowned": icon_unowned,
    "vs_gold": icon_gold,
    "vs_silver": icon_silver,
    "vs_bronze": icon_bronze,
    "vs_players": icon_players,
    "vs_day": icon_day,
    "vs_night": icon_night,
    "vs_wind": icon_wind,
    "vs_fuel": icon_fuel,
    "vs_carrier": icon_carrier,
    "vs_play": icon_play,
    "vs_skip": icon_skip,
    "vs_stop": icon_stop,
    "vs_prev": icon_prev,
    "vs_next": icon_next,
    "vs_queue": icon_queue,
    "vs_music": icon_music,
    "vs_volume": icon_volume,
    "vs_shuffle": icon_shuffle,
    "vs_filter": icon_filter,
    "vs_globe": icon_globe,
    "vs_ticket": icon_ticket,
    "vs_lock": icon_lock,
    "vs_mail": icon_mail,
    "vs_help": icon_help,
    "vs_rules": icon_rules,
    "vs_rocket": icon_rocket,
    "vs_wings": icon_wings,
    "vs_keyboard": icon_keyboard,
    "vs_book": icon_book,
}


def _preview() -> None:
    """Contact sheet of the whole set on a dark ground.

    Committed alongside the PNGs because an icon that reads fine at 128px can
    be mud at the 32px Discord actually renders it at, and the only way to
    catch that is to look at them together.
    """
    cols, cell, lab = 8, 96, 18
    names = list(ICONS)
    rows = (len(names) + cols - 1) // cols
    sheet = Image.new("RGBA", (cols * cell, rows * (cell + lab)), (30, 34, 40, 255))
    d = ImageDraw.Draw(sheet)
    for i, name in enumerate(names):
        r, c = divmod(i, cols)
        glyph = Image.open(os.path.join(OUT_DIR, f"{name}.png")).resize(
            (cell - 16, cell - 16), Image.LANCZOS)
        sheet.alpha_composite(glyph, (c * cell + 8, r * (cell + lab) + 4))
        d.text((c * cell + 4, r * (cell + lab) + cell - 6),
               name.replace("vs_", ""), fill=(200, 208, 216, 255))
    sheet.save(os.path.join(OUT_DIR, "_preview.png"))
    print("  _preview.png")


def main():
    print(f"Rendering {len(ICONS)} icons to {OUT_DIR}")
    for name, fn in ICONS.items():
        _save(fn(), name)
    _preview()
    print("done -- install with /feops icons_install")


if __name__ == "__main__":
    main()
