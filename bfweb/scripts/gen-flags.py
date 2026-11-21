"""Draw the Modern server's flags: NATO, and Russia, Syria, Iran and Iraq.

Writes bfsite/public/media/flag-nato.png and flag-rsii.png, flat: the
public site masks them and lays its own sheen on top. (The dashboard used to
fly flags too; it has wireframe art now -- scripts/gen-wireframes.py.)

The red side is a coalition of four, so its picture is a 2x2 block:
Russia and Iran on the inside, where the fade eats most of the detail (their
colours say who they are), Syria and Iraq on the outside, where their
emblems -- the only thing telling the two apart -- stay readable.

Needs Pillow, and Windows' Arial and Segoe UI Symbol fonts.
    python bfweb/scripts/gen-flags.py
"""

import math
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[2]
W, H = 720, 480
SS = 3  # supersampling, for clean edges on the stars and the compass
FONTS = Path("C:/Windows/Fonts")


def rect(d, x, y, w, h, fill):
    d.rectangle([x, y, x + w, y + h], fill=fill)


def stripes(d, x, y, w, h, colours):
    band = h / len(colours)
    for i, c in enumerate(colours):
        rect(d, x, y + i * band, w, band + 1, c)


def star(d, cx, cy, r, fill):
    pts = []
    for i in range(10):
        a = -math.pi / 2 + i * math.pi / 5
        rr = r if i % 2 == 0 else r * 0.382
        pts.append((cx + rr * math.cos(a), cy + rr * math.sin(a)))
    d.polygon(pts, fill=fill)


def centred_text(d, cx, cy, text, font, fill):
    l, t, r, b = d.textbbox((0, 0), text, font=font)
    d.text((cx - (l + r) / 2, cy - (t + b) / 2), text, font=font, fill=fill)


def nato(d, x, y, w, h):
    blue, white = "#004990", "#ffffff"
    rect(d, x, y, w, h, blue)
    cx, cy = x + w / 2, y + h / 2
    s = h / 480
    tip, inner, ring = 130 * s, 30 * s, 72 * s
    # The lines run out from the compass points toward the edges.
    lw = 5 * s
    gap = tip + 16 * s
    rect(d, cx - lw / 2, y + 26 * s, lw, h / 2 - gap - 26 * s, white)
    rect(d, cx - lw / 2, cy + gap, lw, h / 2 - gap - 26 * s, white)
    rect(d, x + 40 * s, cy - lw / 2, w / 2 - gap - 40 * s, lw, white)
    rect(d, cx + gap, cy - lw / 2, w / 2 - gap - 40 * s, lw, white)
    d.ellipse([cx - ring, cy - ring, cx + ring, cy + ring], outline=white, width=round(8 * s))
    # Four-pointed star, each point half white and half outlined.
    for k in range(4):
        a = k * math.pi / 2 - math.pi / 2
        t = (cx + tip * math.cos(a), cy + tip * math.sin(a))
        p = (cx + inner * math.cos(a - math.pi / 4), cy + inner * math.sin(a - math.pi / 4))
        q = (cx + inner * math.cos(a + math.pi / 4), cy + inner * math.sin(a + math.pi / 4))
        d.polygon([(cx, cy), t, p], fill=white)
        d.polygon([(cx, cy), t, q], fill=blue, outline=white, width=round(4 * s))


def russia(d, x, y, w, h):
    stripes(d, x, y, w, h, ["#ffffff", "#0039a6", "#d52b1e"])


def syria(d, x, y, w, h):
    stripes(d, x, y, w, h, ["#ce1126", "#ffffff", "#000000"])
    for fx in (1 / 3, 2 / 3):
        star(d, x + w * fx, y + h / 2, h * 0.13, "#007a3d")


def iraq(d, x, y, w, h):
    stripes(d, x, y, w, h, ["#ce1126", "#ffffff", "#000000"])
    # "Allahu akbar", in presentation forms laid out left to right: Pillow
    # here has no Arabic shaping, so the joined forms are given directly.
    font = ImageFont.truetype(str(FONTS / "arialbd.ttf"), round(h * 0.2))
    centred_text(d, x + w / 2, y + h / 2, "\ufeae\ufe92\ufedb\ufe83 \ufdf2", font, "#007a3d")


def iran(d, x, y, w, h):
    green, red = "#239f40", "#da0000"
    stripes(d, x, y, w, h, [green, "#ffffff", red])
    # The takbir along both edges of the white band, as a band of marks.
    band = h / 3
    mark_h = band * 0.09
    n = 11
    for i in range(n * 3):
        mx = x + (i + 0.25) * w / (n * 3)
        mw = w / (n * 3) * (0.5 if i % 3 != 1 else 0.25)
        tall = mark_h * (2 if i % 3 == 1 else 1)
        rect(d, mx, y + band - tall - band * 0.04, mw, tall, "#ffffff")
        rect(d, mx, y + 2 * band + band * 0.04, mw, tall, "#ffffff")
    font = ImageFont.truetype(str(FONTS / "seguisym.ttf"), round(h * 0.27))
    centred_text(d, x + w / 2, y + h / 2, "\u262b", font, red)


def draw(paint):
    img = Image.new("RGBA", (W * SS, H * SS), (0, 0, 0, 0))
    paint(ImageDraw.Draw(img), W * SS, H * SS)
    return img.resize((W, H), Image.LANCZOS)


def nato_flag(d, w, h):
    nato(d, 0, 0, w, h)


def rsii_flag(d, w, h):
    gap = 8 * SS
    cw, ch = (w - gap) / 2, (h - gap) / 2
    russia(d, 0, 0, cw, ch)
    syria(d, cw + gap, 0, cw, ch)
    iran(d, 0, ch + gap, cw, ch)
    iraq(d, cw + gap, ch + gap, cw, ch)


def main():
    site = ROOT / "bfsite/public/media"
    for name, paint in [("nato", nato_flag), ("rsii", rsii_flag)]:
        draw(paint).save(site / f"flag-{name}.png", optimize=True)
        print(f"flag-{name}: {site}")


if __name__ == "__main__":
    main()
