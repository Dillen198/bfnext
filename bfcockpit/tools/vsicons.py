"""The Vector Strike icon set, drawn in code.

Tactical line icons on a 24-unit grid: one stroke weight, rounded ends, solid
fills used sparingly for emphasis, and the logo's forward-slash arrow where an
icon means "act". Every icon is white on transparent; the wheel tints it.

    python bfcockpit/tools/vsicons.py <out dir> [size]     # writes <name>.png
    python bfcockpit/tools/vsicons.py --sheet <out.png>    # contact sheet
"""
import math, os, sys
from PIL import Image, ImageDraw

SS = 4          # supersampling
G = 24.0        # design grid
STROKE = 1.9    # stroke width in grid units


class Pen:
    def __init__(self, size):
        self.size = size
        self.big = size * SS
        self.u = self.big / G
        self.img = Image.new("L", (self.big, self.big), 0)
        self.d = ImageDraw.Draw(self.img)

    def p(self, x, y):
        return (x * self.u, y * self.u)

    def dot(self, x, y, r):
        cx, cy = self.p(x, y)
        r = r * self.u
        self.d.ellipse((cx - r, cy - r, cx + r, cy + r), fill=255)

    def line(self, *pts, w=STROKE, closed=False):
        pts = [self.p(*q) for q in pts]
        if closed:
            pts = pts + [pts[0]]
        self.d.line(pts, fill=255, width=int(w * self.u), joint="curve")
        for q in pts:
            r = w * self.u / 2
            self.d.ellipse((q[0] - r, q[1] - r, q[0] + r, q[1] + r), fill=255)

    def fill(self, *pts):
        self.d.polygon([self.p(*q) for q in pts], fill=255)

    def erase(self, *pts):
        self.d.polygon([self.p(*q) for q in pts], fill=0)

    def rect(self, x0, y0, x1, y1, r=1.2, w=STROKE, filled=False):
        box = (*self.p(x0, y0), *self.p(x1, y1))
        if filled:
            self.d.rounded_rectangle(box, radius=r * self.u, fill=255)
        else:
            self.d.rounded_rectangle(box, radius=r * self.u, outline=255, width=int(w * self.u))

    def circle(self, x, y, r, w=STROKE, filled=False):
        cx, cy = self.p(x, y)
        rr = r * self.u
        box = (cx - rr, cy - rr, cx + rr, cy + rr)
        if filled:
            self.d.ellipse(box, fill=255)
        else:
            self.d.ellipse(box, outline=255, width=int(w * self.u))

    def arc(self, x, y, r, a0, a1, w=STROKE):
        pts = []
        steps = max(8, int(abs(a1 - a0) / 4))
        for i in range(steps + 1):
            a = math.radians(a0 + (a1 - a0) * i / steps)
            pts.append((x + r * math.cos(a), y + r * math.sin(a)))
        self.line(*pts, w=w)

    def clear_circle(self, x, y, r):
        cx, cy = self.p(x, y)
        rr = r * self.u
        self.d.ellipse((cx - rr, cy - rr, cx + rr, cy + rr), fill=0)

    def arrowhead(self, x, y, ang, s=3.2):
        """A solid VS-style arrowhead pointing along `ang` degrees."""
        a = math.radians(ang)
        tip = (x, y)
        back = (x - s * math.cos(a), y - s * math.sin(a))
        nx, ny = -math.sin(a), math.cos(a)
        self.fill(tip, (back[0] + nx * s * 0.62, back[1] + ny * s * 0.62),
                  (back[0] - nx * s * 0.62, back[1] - ny * s * 0.62))

    def done(self):
        return self.img.resize((self.size, self.size), Image.LANCZOS)


def badge(pen, kind):
    """A small round badge at the top right: plus, cross or check."""
    pen.clear_circle(18, 6, 5.4)
    pen.circle(18, 6, 4.2, filled=True)
    pen.d.ellipse((0, 0, 0, 0))
    inner = Pen(pen.size)  # draw the symbol knocked out of the badge
    if kind == "plus":
        pen_lines = [((18, 3.6), (18, 8.4)), ((15.6, 6), (20.4, 6))]
    else:
        pen_lines = [((16.3, 4.3), (19.7, 7.7)), ((19.7, 4.3), (16.3, 7.7))]
    for a, b in pen_lines:
        pa, pb = pen.p(*a), pen.p(*b)
        pen.d.line([pa, pb], fill=0, width=int(1.5 * pen.u))


def crate(pen, y0=8, open_lid=False):
    """A braced military crate: frame, lid band, one diagonal brace."""
    pen.rect(3.5, y0, 20.5, 20.5, r=1.0)
    pen.line((3.5, y0 + 3.6), (20.5, y0 + 3.6))
    pen.line((4.6, 20.2), (19.4, y0 + 4.6), w=STROKE * 0.85)
    for x in (6.2, 17.8):
        pen.dot(x, y0 + 1.8, 0.75)


ICONS = {}


def icon(name):
    def deco(fn):
        ICONS[name] = fn
        return fn
    return deco


@icon("crate")
def _(p):
    crate(p)


@icon("crate_open")
def _(p):
    p.rect(3.5, 11, 20.5, 20.5, r=1.0)
    p.line((6.5, 20.5), (11, 11), w=STROKE * 0.85)
    # lid tipped open
    p.line((3.2, 8.6), (13.5, 4.2), (20.8, 7.4))
    p.line((12, 9.5), (12, 2.5))
    p.arrowhead(12, 1.6, -90, 3.4)


@icon("load")
def _(p):
    crate(p, y0=11)
    p.line((12, 9), (12, 2.6))
    p.arrowhead(12, 1.4, -90, 3.4)


@icon("unload")
def _(p):
    crate(p, y0=11)
    p.line((12, 1.6), (12, 7.4))
    p.arrowhead(12, 9.6, 90, 3.4)


@icon("spawn")
def _(p):
    crate(p)
    badge(p, "plus")


@icon("destroy")
def _(p):
    crate(p)
    badge(p, "cross")


@icon("radar_sweep")
def _(p):
    p.arc(12, 12, 9, 200, 340)
    p.arc(12, 12, 5.5, 200, 340)
    p.line((12, 12), (19.5, 6.5))
    p.dot(12, 12, 1.4)
    p.dot(7.5, 8.2, 1.3)
    p.line((3, 19.5), (21, 19.5))


@icon("list")
def _(p):
    p.rect(4.5, 3.5, 19.5, 21, r=1.2)
    p.rect(9, 2, 15, 5.2, r=0.8, filled=True)
    for y in (9.5, 13.5, 17.5):
        p.dot(8.2, y, 1.0)
        p.line((11, y), (16.5, y))


@icon("troops")
def _(p):
    # two figures, the nearer one in front and knocked out of the one behind
    def figure(cx, top):
        p.circle(cx, top + 3.4, 3.3, filled=True)
        p.fill(*[(cx + 6.2 * math.cos(math.radians(a)), top + 15.6 + 6.2 * math.sin(math.radians(a))) for a in range(180, 361, 10)])
    figure(15.6, 2.2)
    p.clear_circle(9.4, 7.4, 4.9)
    p.erase(*[(9.4 + 7.8 * math.cos(math.radians(a)), 21.4 + 7.8 * math.sin(math.radians(a))) for a in range(180, 361, 10)])
    figure(9.4, 4.0)
    p.erase((0, 21.4), (24, 21.4), (24, 24), (0, 24))


@icon("csar")
def _(p):
    p.arc(12, 11, 9, 195, 345)
    p.line((3.3, 8.7), (20.7, 8.7), w=STROKE * 0.8)
    for x in (3.4, 8.2, 15.8, 20.6):
        p.line((x, 9), (12, 16.5), w=STROKE * 0.7)
    p.circle(12, 17.6, 1.4, filled=True)
    p.line((12, 18.8), (12, 21.4))
    p.line((9.8, 20), (14.2, 20))


@icon("jtac")
def _(p):
    p.circle(12, 12, 7.2)
    for a in (0, 90, 180, 270):
        x0, y0 = 12 + 4.6 * math.cos(math.radians(a)), 12 + 4.6 * math.sin(math.radians(a))
        x1, y1 = 12 + 10.6 * math.cos(math.radians(a)), 12 + 10.6 * math.sin(math.radians(a))
        p.line((x0, y0), (x1, y1))
    p.dot(12, 12, 1.6)


@icon("target")
def _(p):
    p.circle(12, 12, 9)
    p.circle(12, 12, 5)
    p.dot(12, 12, 1.8)


@icon("smoke")
def _(p):
    for x, h in ((7, 0), (12, 2), (17, 1)):
        pts = [(x + 1.6 * math.sin((y + h) / 2.2), y) for y in [18 - i * 1.5 for i in range(10)]]
        p.line(*pts, w=STROKE * 0.95)
    p.line((3, 20.5), (21, 20.5))


@icon("flag")
def _(p):
    p.line((6, 21), (6, 3))
    p.fill((6.8, 3.4), (19, 3.4), (16, 7.6), (19, 11.8), (6.8, 11.8))
    p.line((3, 21), (12, 21))


@icon("info")
def _(p):
    p.circle(12, 12, 9.2)
    p.dot(12, 7.4, 1.4)
    p.line((12, 11), (12, 17))
    p.line((10.4, 11), (12, 11))


@icon("bolt")
def _(p):
    p.fill((13.8, 2), (5, 13.4), (11.2, 13.4), (9.6, 22), (19, 10), (12.6, 10), (15, 2))


@icon("launcher")
def _(p):
    # a missile on its launch rail
    p.line((3, 21), (21, 21))
    p.fill((5, 21), (8.6, 21), (12.4, 15.4), (9.6, 13.6))
    p.line((14, 21), (11.6, 16.6), w=STROKE * 0.9)
    ang = math.radians(-42)
    ux, uy = math.cos(ang), math.sin(ang)
    nx, ny = -uy, ux
    x0, y0 = 6.6, 15.0
    L, W = 13.6, 1.75
    body = [(x0 + nx * W, y0 + ny * W), (x0 + ux * L + nx * W, y0 + uy * L + ny * W),
            (x0 + ux * (L + 3.4), y0 + uy * (L + 3.4)),
            (x0 + ux * L - nx * W, y0 + uy * L - ny * W), (x0 - nx * W, y0 - ny * W)]
    p.fill(*body)
    for sgn in (1, -1):
        p.fill((x0 + nx * W * sgn, y0 + ny * W * sgn), (x0 + nx * 4.2 * sgn - ux * 1.2, y0 + ny * 4.2 * sgn - uy * 1.2),
               (x0 + ux * 3 + nx * W * sgn, y0 + uy * 3 + ny * W * sgn))


@icon("jet")
def _(p):
    p.fill((12, 1.8), (13.3, 6), (13.3, 9.6), (21.5, 14.2), (21.5, 16), (13.3, 13.6), (13, 18.4),
           (16, 20.6), (16, 22), (12, 21), (8, 22), (8, 20.6), (11, 18.4), (10.7, 13.6),
           (2.5, 16), (2.5, 14.2), (10.7, 9.6), (10.7, 6))


@icon("helo")
def _(p):
    p.line((2.5, 5.2), (21.5, 5.2))
    p.line((12, 5.2), (12, 8.2))
    p.fill((6, 9), (14.5, 9), (17, 12.5), (16.2, 15.2), (7.4, 15.2), (5, 12.5))
    p.line((15.5, 12), (21.5, 10), w=STROKE * 0.95)
    p.line((21.5, 8.2), (21.5, 11.6), w=STROKE * 0.8)
    p.line((6.5, 18.4), (16.5, 18.4))
    p.line((9, 15.2), (8.2, 18.4), w=STROKE * 0.8)
    p.line((14, 15.2), (14.8, 18.4), w=STROKE * 0.8)


@icon("transport")
def _(p):
    # high-wing, four-engine transport, top view
    p.fill((11, 2), (13, 2), (13.6, 5), (13.6, 19), (12, 22), (10.4, 19), (10.4, 5))
    p.fill((2, 9), (22, 9), (22, 11.4), (2, 11.4))
    for x in (4.6, 8, 16, 19.4):
        p.fill((x - 0.8, 7.4), (x + 0.8, 7.4), (x + 0.8, 9.4), (x - 0.8, 9.4))
    p.fill((7.6, 18.6), (16.4, 18.6), (16.4, 20.2), (7.6, 20.2))


@icon("ewr")
def _(p):
    # a dish on a mast
    p.arc(11, 9, 7, 110, 290)
    p.line((7.6, 15.1), (16.2, 4.4))
    p.line((12, 9.8), (16.6, 13.8))
    p.dot(17.4, 14.5, 1.3)
    p.line((10, 13.5), (10, 21))
    p.line((6, 21), (14, 21))


@icon("recon")
def _(p):
    pts_top = [(2.5 + i, 12 - 6.4 * math.sin(math.pi * i / 19)) for i in range(20)]
    pts_bot = [(2.5 + i, 12 + 6.4 * math.sin(math.pi * i / 19)) for i in range(20)]
    p.line(*pts_top)
    p.line(*pts_bot)
    p.circle(12, 12, 3.6, w=STROKE * 0.9)
    p.dot(12, 12, 1.5)


@icon("wrench")
def _(p):
    p.line((5, 19), (14.4, 9.6), w=STROKE * 1.5)
    p.circle(16.6, 7.4, 4.4)
    p.erase((16.6, 7.4), (21.8, 2.2), (23.5, 4), (18.3, 9.1))
    p.dot(5, 19, 1.2)


@icon("fuel")
def _(p):
    p.fill((5.5, 6.5), (13, 6.5), (18.5, 11), (18.5, 21), (5.5, 21))
    p.erase((8, 12.4), (16, 12.4), (16, 18.6), (8, 18.6))
    p.line((8.2, 12.6), (15.8, 18.4), w=STROKE * 0.8)
    p.line((15.8, 12.6), (8.2, 18.4), w=STROKE * 0.8)
    p.line((7, 3.2), (11, 3.2), (11, 6.5), w=STROKE * 0.9)
    p.line((15, 5), (19.5, 3), w=STROKE * 0.9)


@icon("bomb")
def _(p):
    p.fill((10, 3), (14, 3), (15.4, 6.4), (15.4, 15.6), (12, 20), (8.6, 15.6), (8.6, 6.4))
    p.fill((8.6, 3), (10.4, 3), (8.6, 6.4))
    p.fill((15.4, 3), (13.6, 3), (15.4, 6.4))
    p.line((6.5, 1.8), (17.5, 1.8))
    p.line((12, 20.5), (12, 22.5), w=STROKE * 0.9)


@icon("hangar")
def _(p):
    p.arc(12, 20, 9.5, 180, 360)
    p.line((2.5, 20.5), (21.5, 20.5))
    p.rect(8, 13.5, 16, 20.5, r=0.4)
    p.line((12, 13.5), (12, 20.5), w=STROKE * 0.7)


@icon("tank")
def _(p):
    p.rect(2.5, 13.5, 21.5, 19.5, r=3)
    for x in (6.5, 10.5, 14.5, 18.5):
        p.dot(x, 16.5, 1.1)
    p.fill((6, 13.5), (8, 9.2), (16, 9.2), (18, 13.5))
    p.line((15, 10.6), (22, 8.2), w=STROKE * 0.95)


@icon("panel")
def _(p):
    p.rect(3, 3, 21, 21, r=1.6)
    p.rect(6.5, 6.5, 17.5, 17.5, r=0.6, w=STROKE * 0.8)
    for y in (8.5, 12, 15.5):
        p.line((3.6, y), (5, y), w=STROKE * 0.75)
        p.line((19, y), (20.4, y), w=STROKE * 0.75)
    p.line((9, 14.5), (11.5, 11), (13.4, 12.6), (15.4, 9.4), w=STROKE * 0.8)


@icon("chat")
def _(p):
    p.rect(2.5, 3.5, 21.5, 16.5, r=2.2)
    p.fill((6.5, 15.5), (11.2, 15.5), (6.5, 21))
    for x in (7.5, 12, 16.5):
        p.dot(x, 10, 1.2)


@icon("clock")
def _(p):
    p.circle(12, 13, 8.6)
    p.line((12, 8.4), (12, 13), (15.6, 15.2))
    p.line((9.5, 2.4), (14.5, 2.4))
    p.line((12, 2.4), (12, 4.4))


@icon("compass")
def _(p):
    p.circle(12, 12, 9.4)
    p.fill((12, 4.4), (14.4, 12), (12, 10.8), (9.6, 12))
    p.line((9.6, 12), (12, 19.6), (14.4, 12), w=STROKE * 0.7)
    p.dot(12, 12, 1.1)


@icon("medal")
def _(p):
    p.fill((6, 2), (10, 2), (13.5, 9.5), (10.5, 9.5))
    p.fill((18, 2), (14, 2), (10.5, 9.5), (13.5, 9.5))
    p.circle(12, 15, 6.2)
    star = []
    for i in range(10):
        r = 3.6 if i % 2 == 0 else 1.5
        a = math.radians(-90 + i * 36)
        star.append((12 + r * math.cos(a), 15.2 + r * math.sin(a)))
    p.fill(*star)


@icon("close")
def _(p):
    p.line((6, 6), (18, 18))
    p.line((18, 6), (6, 18))


@icon("more")
def _(p):
    for x in (5.5, 12, 18.5):
        p.dot(x, 12, 2.1)


@icon("back")
def _(p):
    p.line((14.5, 5), (7.5, 12), (14.5, 19))


@icon("group")
def _(p):
    for x, y in ((4, 4), (13, 4), (4, 13), (13, 13)):
        p.rect(x, y, x + 7, y + 7, r=1.1)
    p.rect(14.6, 14.6, 18.4, 18.4, r=0.6, filled=True)


@icon("gear")
def _(p):
    p.circle(12, 12, 6.2)
    p.circle(12, 12, 2.4, w=STROKE * 0.9)
    for i in range(8):
        a = math.radians(i * 45)
        p.line((12 + 6.4 * math.cos(a), 12 + 6.4 * math.sin(a)),
               (12 + 9.4 * math.cos(a), 12 + 9.4 * math.sin(a)), w=STROKE * 1.2)


@icon("star")
def _(p):
    pts = []
    for i in range(10):
        r = 9.6 if i % 2 == 0 else 4.2
        a = math.radians(-90 + i * 36)
        pts.append((12 + r * math.cos(a), 12.6 + r * math.sin(a)))
    p.line(*pts, closed=True)


@icon("generic")
def _(p):
    p.line((12, 3), (21, 12), (12, 21), (3, 12), closed=True)
    p.dot(12, 12, 2.2)


@icon("vector")
def _(p):
    # the logo's arrow, for anything that means "go"
    p.line((4, 20), (17.5, 6.5), w=STROKE * 1.1)
    p.arrowhead(20.8, 3.2, -45, 5.2)
    p.line((4, 13), (8.5, 8.5), w=STROKE * 0.8)
    p.line((11, 20), (15.5, 15.5), w=STROKE * 0.8)


def render(name, size):
    pen = Pen(size)
    ICONS[name](pen)
    m = pen.done()
    img = Image.new("RGBA", m.size, (255, 255, 255, 0))
    img.putalpha(m)
    return img


def write_all(out_dir, size=64):
    os.makedirs(out_dir, exist_ok=True)
    for name in ICONS:
        render(name, size).save(os.path.join(out_dir, name + ".png"), optimize=True)
    return list(ICONS)


def sheet(out, size=64, cols=8):
    names = list(ICONS)
    rows = math.ceil(len(names) / cols)
    cell = size + 52
    img = Image.new("RGBA", (cols * cell, rows * cell + 10), (13, 16, 9, 255))
    from PIL import ImageFont
    try:
        f = ImageFont.truetype(r"D:/DCS World/dxgui/skins/fonts/DejaVuLGCSansCondensed.ttf", 12)
    except OSError:
        f = None
    d = ImageDraw.Draw(img)
    for i, n in enumerate(names):
        x, y = (i % cols) * cell, (i // cols) * cell
        ic = render(n, size)
        tint = Image.new("RGBA", ic.size, (205, 212, 190, 0))
        tint.putalpha(ic.split()[3])
        img.alpha_composite(tint, (x + (cell - size) // 2, y + 10))
        d.text((x + cell / 2, y + size + 26), n, fill=(142, 200, 63), font=f, anchor="mm")
    img.convert("RGB").save(out)


if __name__ == "__main__":
    if sys.argv[1] == "--sheet":
        sheet(sys.argv[2])
    else:
        print(write_all(sys.argv[1], int(sys.argv[2]) if len(sys.argv) > 2 else 64))
