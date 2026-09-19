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
    d.rectangle([cx - W * 0.5, S * 0.44, cx + W * 0.5, S * 0.70], fill=(18, 22, 28, 255))
    d.ellipse([cx - W * 0.5, S * 0.76, cx + W * 0.5, S * 0.76 + W], fill=(18, 22, 28, 255))
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


def icon_live():
    img, d = _canvas()
    cx = cy = S / 2
    r = (S / 2 - PAD) * 0.62
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=GREEN)
    _ring(d, cx, cy, r * 1.62, (*GREEN[:3], 110), w=int(W * 0.6))
    return img


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
}


def _preview() -> None:
    """Contact sheet of the whole set on a dark ground.

    Committed alongside the PNGs because an icon that reads fine at 128px can
    be mud at the 32px Discord actually renders it at, and the only way to
    catch that is to look at them together.
    """
    cols, cell, lab = 6, 96, 18
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
