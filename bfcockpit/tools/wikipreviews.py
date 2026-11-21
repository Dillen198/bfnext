"""Preview images of the Vector Strike panel, wheel and settings for the wiki.

    python bfcockpit/tools/wikipreviews.py <background.jpg> <out dir>

Drawn with the panel's own colours, fonts and layout (Scripts/Hooks/
bfcockpit.lua) and the wheel's real generated art (Theme/wheel), filled with
sample data, so players can see what each part looks like before they fly.
Re-run when the panel's look changes; the images are committed under
bfdb/seed_wiki/images.
"""
import math, os, sys
from PIL import Image, ImageDraw, ImageFilter, ImageFont

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import vsicons

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
THEME = os.path.join(ROOT, "Mods", "Services", "BFCockpit", "Theme")
FONTS = r"D:/DCS World/dxgui/skins/fonts/"
K = 1.25   # draw at 125% so the wiki images are crisp

C = dict(bg=(13, 16, 9), header=(26, 34, 18), text=(205, 212, 190), dim=(125, 138, 104),
         accent=(142, 200, 63), bad=(224, 108, 108), blue=(111, 168, 220), btn=(39, 52, 30),
         btn_off=(20, 25, 15), border=(58, 74, 46), answer=(29, 42, 20), warn=(224, 160, 80))


def font(size, bold=False, mono=False):
    name = "DejaVuLGCSansMono.ttf" if mono else ("DejaVuLGCSansCondensed-Bold.ttf" if bold else "DejaVuLGCSansCondensed.ttf")
    return ImageFont.truetype(FONTS + name, int(size * K))


def px(v):
    return int(round(v * K))


class Panel:
    """One panel window, drawn the way layout()/draw_rows() place things."""

    W, H = 560, 600

    def __init__(self, tab, crumb=None, second=None):
        self.img = Image.new("RGBA", (px(self.W), px(self.H)), C["bg"] + (245,))
        self.d = ImageDraw.Draw(self.img)
        self.tab = tab
        d = self.d
        # header
        d.rectangle((0, 0, px(self.W), px(24)), fill=C["header"])
        d.text((px(8), px(12)), "Vector Strike", font=font(13, True), fill=C["accent"], anchor="lm")
        self.top = 24
        pad, row = 8, 26
        # tabs + tools
        tools = ["-", "+", "X"]
        rx = self.W - pad - 26
        for t in reversed(tools):
            self.button(rx, self.top + pad, 26, row, t, center=True)
            rx -= 30
        tabs = ["MENU", "CARGO", "STATUS", "EWR", "LOG", "WHEEL"]
        tw = min(80, (rx - pad) // len(tabs) - 4)
        x = pad
        for t in tabs:
            self.button(x, self.top + pad, tw, row, t, center=True, on=(t == tab))
            x += tw + 4
        y2 = self.top + pad + row + 6
        self.y = y2
        if crumb is not None:
            self.button(pad, y2, 64, row, "< Back", center=True, enabled=crumb != "F10 > Other")
            d.text((px(pad + 72), px(y2 + row / 2)), crumb, font=font(13), fill=C["dim"], anchor="lm")
            self.y = y2 + row + 6
        elif second:
            bw = (self.W - pad * 2 - 8) // len(second)
            for i, (label, on) in enumerate(second):
                self.button(pad + i * (bw + 4), y2, bw, row, label, center=True, on=on)
            self.y = y2 + row + 6
        # pane
        self.pane = (pad, self.y, self.W - pad, self.H - self.top - pad - 24)
        d.rectangle((px(self.pane[0]), px(self.pane[1] + self.top - self.top), px(self.pane[2]), px(self.pane[3] + self.top)),
                    outline=C["border"], width=max(1, px(1)))
        self.cy = self.y + 4
        self.cx0 = pad + 4
        self.cw = self.W - pad * 2 - 38

    def button(self, x, y, w, h, text, center=False, on=False, enabled=True, go=False, skin_sub=False):
        bg = C["accent"] if (on or go) else (C["btn"] if enabled else C["btn_off"])
        fg = C["bg"] if (on or go) else (C["dim"] if not enabled else (C["accent"] if skin_sub else C["text"]))
        self.d.rectangle((px(x), px(y), px(x + w), px(y + h)), fill=bg)
        f = font(13, bold=center or skin_sub or on)
        if center:
            self.d.text((px(x + w / 2), px(y + h / 2)), text, font=f, fill=fg, anchor="mm")
        else:
            self.d.text((px(x + 4), px(y + h / 2)), text, font=f, fill=fg, anchor="lm")

    # rows inside the pane ------------------------------------------------
    def row_button(self, text, sub=False, x=0, w=None, center=False, on=False, go=False, same=False, enabled=True):
        if same:
            self.cy -= 29
        w = w or self.cw - x
        self.button(self.cx0 + x, self.cy, w, 26, text, center=center, on=on, go=go, skin_sub=sub, enabled=enabled)
        self.cy += 29

    def text(self, s, color="text", size=13, bold=False, x=0, mono=False, same=False, h=19):
        if same:
            self.cy -= h + 3
        self.d.text((px(self.cx0 + x), px(self.cy + h / 2)), s, font=font(size, bold, mono), fill=C[color], anchor="lm")
        self.cy += h + 3

    def answer(self, at, lines):
        self.text("  " + at + "  -- the server says:", "dim")
        for l in lines:
            self.d.rectangle((px(self.cx0), px(self.cy), px(self.cx0 + self.cw), px(self.cy + 19)), fill=C["answer"])
            self.text("  " + l)
        self.cy += 6

    def icon(self, name, x, size=20):
        ic = vsicons.render(name, px(size))
        t = Image.new("RGBA", ic.size, C["text"] + (0,))
        t.putalpha(ic.split()[3])
        self.img.alpha_composite(t, (px(self.cx0 + x), px(self.cy + 1)))

    def status(self, s, color="dim"):
        y = self.H - 8 - 20
        em = Image.open(os.path.join(THEME, "emblem-64.png")).convert("RGBA").resize((px(18), px(18)), Image.LANCZOS)
        self.img.alpha_composite(em, (px(8), px(y + 1)))
        self.d.text((px(30), px(y + 10)), s, font=font(12), fill=C[color], anchor="lm")

    def kv(self, k, v, color="text", bold=False):
        self.text(k, "dim")
        self.text(v, color, x=110, same=True, bold=bold)


def on_background(panel_img, bg, x=None, y=None, scale=1.0):
    bg = bg.copy()
    if scale != 1.0:
        panel_img = panel_img.resize((int(panel_img.size[0] * scale), int(panel_img.size[1] * scale)), Image.LANCZOS)
    x = (bg.size[0] - panel_img.size[0]) // 2 if x is None else x
    y = (bg.size[1] - panel_img.size[1]) // 2 if y is None else y
    shadow = Image.new("RGBA", bg.size, (0, 0, 0, 0))
    ImageDraw.Draw(shadow).rectangle((x + 6, y + 8, x + panel_img.size[0] + 6, y + panel_img.size[1] + 8), fill=(0, 0, 0, 140))
    bg.alpha_composite(shadow.filter(ImageFilter.GaussianBlur(10)))
    bg.alpha_composite(panel_img, (x, y))
    return bg


def backdrop(path, size):
    bg = Image.open(path).convert("RGBA").resize(size, Image.LANCZOS).filter(ImageFilter.GaussianBlur(3))
    bg.alpha_composite(Image.new("RGBA", size, (0, 0, 0, 110)))
    return bg


def callout(img, x, y, n):
    """A numbered marker for the wiki text to refer to."""
    d = ImageDraw.Draw(img)
    r = 13
    d.ellipse((x - r, y - r, x + r, y + r), fill=(224, 160, 80, 255), outline=(13, 16, 9, 255), width=2)
    d.text((x, y), str(n), font=ImageFont.truetype(FONTS + "DejaVuLGCSansCondensed-Bold.ttf", 16), fill=(13, 16, 9), anchor="mm")


# ── the panel tabs ─────────────────────────────────────────────────────────

def menu_tab():
    p = Panel("MENU", crumb="F10 > Cargo")
    p.answer("14:02:11", ["SA-3 Launcher crate spawned. 3 more needed to build", "an SA-3 site here."])
    for name in ["Unpack Nearby Crate(s)", "Load Nearby Crate", "Unload Crate", "List Nearby Crates", "List Cargo", "Destroy Nearby Crate"]:
        p.row_button("  " + name, w=p.cw - 34)
        p.row_button("★" if name == "List Cargo" else "☆", x=p.cw - 30, w=30, center=True, same=True)
    p.row_button("  Crates  >", sub=True)
    p.row_button("  CSAR  >", sub=True)
    p.status("Vector Strike 3.0.0  ·  CTRL+ALT+C panel  ·  CTRL+ALT+W wheel")
    return p.img


def cargo_tab():
    p = Panel("CARGO")
    p.text("ON BOARD", "accent", 14, True)
    p.text("Crates 1 / 2    Troops 0 / 8")
    p.text("  • FARP Crate")
    p.cy += 4
    p.text("NEARBY CRATES", "accent", 14, True)
    p.text("  SA-3 Launcher  --  42 m, bearing 270°")
    half = (p.cw - 4) // 2
    for i, a in enumerate(["UNPACK NEARBY CRATE(S)", "LOAD NEARBY CRATE", "UNLOAD CRATE", "LIST CARGO"]):
        p.row_button(a, x=(i % 2) * (half + 4), w=half, center=True, same=(i % 2 == 1))
    p.cy += 4
    p.text("BASE SUPPLY", "accent", 14, True)
    p.text("Repair Kit  --  need 1, 500 kg each", w := None) if False else p.text("Repair Kit  --  need 1, 500 kg each")
    p.row_button("SPAWN 1", x=p.cw - 84, w=84, center=True, same=True)
    p.cy += 4
    p.text("BUILD SOMETHING -- pick a category", "accent", 14, True)
    for cat, n in [("Air Defense", 6), ("Artillery", 3), ("Logistics", 4), ("Armor", 5)]:
        p.row_button(f"  {cat}   ({n})  >", sub=True)
    p.status("Vector Strike 3.0.0")
    return p.img


def cargo_category():
    p = Panel("CARGO", crumb="CARGO > Air Defense")
    p.text("SA-3 / Launcher site   (150 pts)", "accent", 14, True)
    p.text("Needs 5 crates in all, unpacked together:", "dim")
    p.text("  SA-3 Launcher  --  need 4, 900 kg each")
    p.row_button("SPAWN 1", x=p.cw - 184, w=84, center=True, same=True)
    p.row_button("SPAWN ALL 4", x=p.cw - 96, w=96, go=True, center=True, same=True)
    p.text("  SA-3 Radar  --  need 1, 1200 kg each")
    p.row_button("SPAWN 1", x=p.cw - 84, w=84, center=True, same=True)
    p.text("  Repair kit (fixes a damaged one):", "dim")
    p.text("  SA-3 Repair  --  need 1, 300 kg each")
    p.row_button("SPAWN 1", x=p.cw - 84, w=84, center=True, same=True)
    p.cy += 8
    p.text("Avenger   (60 pts)", "accent", 14, True)
    p.text("Needs 2 crates in all, unpacked together:", "dim")
    p.text("  Avenger Crate  --  need 2, 700 kg each")
    p.row_button("SPAWN 1", x=p.cw - 184, w=84, center=True, same=True)
    p.row_button("SPAWN ALL 2", x=p.cw - 96, w=96, go=True, center=True, same=True)
    p.status("Vector Strike 3.0.0")
    return p.img


def status_tab():
    p = Panel("STATUS")
    p.text("№15 | KillerDog", "accent", 14, True)
    p.kv("Coalition", "RED", "bad", True)
    p.kv("Points", "31108")
    p.kv("Lives", "standard 2")
    p.kv("Crates", "1")
    p.cy += 10
    p.text("UH-1H  Huey 1-1", "accent", 14, True)
    p.kv("Position", "N 35°06.000'  E 036°12.000'")
    p.kv("Altitude", "1250 ft")
    p.kv("Heading", "045°  95 kts")
    p.kv("State", "Airborne")
    p.kv("Home", "Hama")
    p.kv("Nearest", "Senaki (blue) 11.9 nm @ 012°", "blue")
    p.kv("Takeoff in", "0:45", "bad")
    p.status("Vector Strike 3.0.0")
    return p.img


def ewr_tab():
    p = Panel("EWR", second=[("THREATS", True), ("FRIENDLIES", False), ("GROUND INTEL", False)])
    third = (p.cw - 8) // 3
    for i, t in enumerate(["EWR CALLS ON/OFF", "IMPERIAL", "METRIC"]):
        p.row_button(t, x=i * (third + 4), w=third, center=True, same=i > 0)
    p.cy += 4
    for l in ["Bandits BRAA", "Group 1  045 / 32 nm / 25k  HOT   2x MiG-29", "Group 2  110 / 58 nm / 8k   COLD  Su-25",
              "Group 3  300 / 71 nm / 31k  FLANK Su-27"]:
        p.text(l, mono=True, size=12, h=16)
    p.status("EWR calls: enabled", "dim")
    return p.img


def log_tab():
    p = Panel("LOG")
    msgs = [("14:02:11", "YOU", "SA-3 Launcher crate spawned. 3 more needed to build an SA-3 site here.", "accent"),
            ("14:01:40", "YOU", "you must land to spawn crates", "accent"),
            ("13:58:02", "SIDE", "Blue has captured Senaki", "blue"),
            ("13:55:17", "ALL", "Server restart in 2 hours", "text"),
            ("13:51:09", "YOU", "Current Cargo: FARP Crate (1/2 crate slots)", "accent")]
    for at, tag, txt, col in msgs:
        p.text(at + "  " + tag, "dim")
        p.text("  " + txt, col)
        p.cy += 4
    p.status("Vector Strike 3.0.0")
    return p.img


def wheel_tab():
    p = Panel("WHEEL")
    p.text("WHEEL FOR UH-1H", "accent", 14, True)
    p.text("Your own layout. Changes apply the next time the wheel opens.", "dim")
    p.row_button("RESET TO DEFAULT", w=150, center=True)
    p.cy += 4
    rows = [("crate", "Cargo   -- F10 menu, opens an arc"), ("troops", "Troops   -- F10 menu, opens an arc"),
            ("crate", "CRATES ABOARD   -- live info"), ("compass", "NEAREST OBJECTIVE   -- live info"),
            ("group", "FAVOURITES   -- 2 in this group"), ("panel", "F10 MENU   -- opens the panel")]
    for icon, label in rows:
        p.icon(icon, 0)
        btns = ["▲", "▼", "ICON", "✕"] if "FAVOURITES" not in label else ["▲", "▼", "ICON", "RENAME", "✕"]
        widths = [30 if b not in ("ICON", "RENAME") else (46 if b == "ICON" else 70) for b in btns]
        total = sum(widths) + 3 * len(widths)
        p.text("  " + label, x=24)
        x = p.cw - total
        for b, w in zip(btns, widths):
            p.row_button(b, x=x, w=w, center=True, same=True)
            x += w + 3
        p.cy += 7
    p.cy += 6
    p.text("ADD TO THE WHEEL", "accent", 14, True)
    q = (p.cw - 9) // 4
    for i, a in enumerate(["+ F10 MENU", "+ LIVE INFO", "+ PANEL", "+ NEW GROUP"]):
        p.row_button(a, x=i * (q + 3), w=q, center=True, same=i > 0, on=(i == 1))
    for w in ["  + NEAREST OBJECTIVE", "  + CRATES ABOARD", "  + TAKEOFF", "  + POINTS"]:
        p.row_button(w)
    p.status("Vector Strike 3.0.0")
    return p.img


# ── the wheel ──────────────────────────────────────────────────────────────

def wheel_image(bg_path, out, hot_ring, arc_hot, labels, arc_labels, hub_title, hub_sub, strip=None, dot_at=None, theme=(142, 200, 63)):
    WHEEL = os.path.join(THEME, "wheel")
    layout = open(os.path.join(WHEEL, "layout.lua"), encoding="utf-8").read()
    import re

    def rect(section, key, after=None):
        m = re.search(section, layout)
        sub = layout[m.end():]
        if after is not None:
            m2 = re.search(r"\[%d\] = \{" % after, sub)
            sub = sub[m2.end():]
        m3 = re.search(r"\[%d\] = \{\s*h = ([\d.]+),\s*w = ([\d.]+),\s*x = ([\d.]+),\s*y = ([\d.]+)," % key, sub)
        h, w, x, y = map(float, m3.groups())
        return x, y, w, h

    S = 1.0
    canvas = Image.new("RGBA", (760, 820), (0, 0, 0, 0))

    def tint(img, rgb, a):
        t = Image.new("RGBA", img.size, rgb + (0,))
        t.putalpha(img.split()[3].point(lambda v: v * a // 255))
        return t

    def place(file, r, rgb, a):
        x, y, w, h = r
        img = Image.open(os.path.join(WHEEL, file)).convert("RGBA").resize((max(1, int(w)), max(1, int(h))), Image.LANCZOS)
        canvas.alpha_composite(tint(img, rgb, a), (int(x), int(y)))

    def label(x, y, s, size, rgb, bold=True):
        ImageDraw.Draw(canvas).text((x, y), s, font=ImageFont.truetype(FONTS + ("DejaVuLGCSansCondensed-Bold.ttf" if bold else "DejaVuLGCSansCondensed.ttf"), size), fill=rgb, anchor="mm")

    def icon(name, x, y, size, rgb):
        ic = vsicons.render(name, size)
        canvas.alpha_composite(tint(ic, rgb, 255), (int(x - size / 2), int(y - size / 2)))

    INK, TEXT, BAD = (13, 16, 9), (205, 212, 190), (224, 108, 108)
    n = len(labels)
    c = 380
    owner = hot_ring if arc_labels else None
    for i, (ic, lab, group, danger) in enumerate(labels, start=1):
        r = rect(r"ring = \{", i, after=n)
        base = f"r{n}_{i}"
        hot = (i == hot_ring and arc_hot is None)
        own = (i == owner)
        if hot:
            place(base + "_glow.png", r, theme, 150)
        place(base + "_fill.png", r, theme if (hot or own) else INK, 240 if hot else (110 if own else 214))
        place(base + "_line.png", r, BAD if danger and not hot else theme, 255 if (hot or own) else 80)
        a = math.radians(-90 + (i - 1) * 360 / n)
        rm = 168
        ix, iy = c + (rm - 6) * math.cos(a), c + (rm - 6) * math.sin(a)
        fg = INK if hot else (BAD if danger else TEXT)
        icon(ic, ix, iy - 9, 40, fg)
        label(ix, iy + 24, lab, 13, fg)
        if group:
            gx, gy = c + 222 * math.cos(a), c + 222 * math.sin(a)
            label(gx, gy, "•", 16, INK if hot else theme)
    if arc_labels:
        m = len(arc_labels)
        mid = -90 + (owner - 1) * 360 / n
        start = round((mid - m * 20 / 2) / 5) * 5
        for j, (ic, lab, danger) in enumerate(arc_labels, start=1):
            a0 = start + (j - 1) * 20
            k = int(round((a0 % 360) / 5)) % 72
            r = rect(r"arc = \{", k)
            hot = (j == arc_hot)
            if hot:
                place(f"a{k}_glow.png", r, theme, 140)
            place(f"a{k}_fill.png", r, theme if hot else INK, 245 if hot else 226)
            place(f"a{k}_line.png", r, BAD if danger else theme, 255 if hot else 100)
            am = math.radians(a0 + 10)
            ix, iy = c + 290 * math.cos(am), c + 290 * math.sin(am)
            fg = INK if hot else (BAD if danger else TEXT)
            icon(ic, ix, iy - 7, 30, fg)
            label(ix, iy + 16, lab, 10, fg)
    hr = re.search(r"hub = \{\s*h = ([\d.]+),\s*w = ([\d.]+),\s*x = ([\d.]+),\s*y = ([\d.]+),", layout).groups()
    h, w, x, y = map(float, hr)
    place("hub_fill.png", (x, y, w, h), INK, 236)
    place("hub_line.png", (x, y, w, h), theme, 200)
    em = Image.open(os.path.join(THEME, "emblem-64.png")).convert("RGBA").resize((56, 56), Image.LANCZOS)
    canvas.alpha_composite(em, (c - 28, c - 74))
    label(c, c + 2, hub_title, 15, theme)
    label(c, c + 24, hub_sub, 11, (138, 150, 120), bold=False)
    if dot_at:
        ang, rr = dot_at
        dx, dy = c + rr * math.cos(math.radians(ang)), c + rr * math.sin(math.radians(ang))
        g = Image.new("RGBA", canvas.size, (0, 0, 0, 0))
        ImageDraw.Draw(g).ellipse((dx - 16, dy - 16, dx + 16, dy + 16), fill=theme + (110,))
        canvas.alpha_composite(g.filter(ImageFilter.GaussianBlur(5)))
        ImageDraw.Draw(canvas).ellipse((dx - 7, dy - 7, dx + 7, dy + 7), fill=theme + (255,), outline=(255, 255, 255, 230), width=2)
    if strip:
        sx, sy = c - 320, 752
        ImageDraw.Draw(canvas).rounded_rectangle((sx, sy, sx + 640, sy + 46), radius=12, fill=INK + (232,), outline=theme + (160,), width=2)
        label(sx + 50, sy + 23, "SERVER", 12, theme)
        ImageDraw.Draw(canvas).text((sx + 96, sy + 23), strip, font=ImageFont.truetype(FONTS + "DejaVuLGCSansCondensed.ttf", 14), fill=TEXT, anchor="lm")
    bg = backdrop(bg_path, (1280, 900))
    bg.alpha_composite(canvas, ((1280 - 760) // 2, 30))
    bg.convert("RGB").save(out, quality=90)


# ── the settings page ──────────────────────────────────────────────────────

def settings_page(out):
    W, H = 1100, 900
    img = Image.new("RGBA", (W, H), (36, 48, 64, 255))
    d = ImageDraw.Draw(img)
    d.rectangle((0, 0, W, 46), fill=(24, 32, 44))
    tabs = ["SYSTEM", "CONTROLS", "GAMEPLAY", "MISC.", "AUDIO", "SPECIAL", "VR"]
    for i, t in enumerate(tabs):
        x = 40 + i * 140
        if t == "SPECIAL":
            d.rectangle((x - 10, 8, x + 110, 44), fill=(52, 70, 92))
        d.text((x + 50, 26), t, font=font(12, True), fill=(190, 205, 220), anchor="mm")
    d.rectangle((0, 46, 210, H), fill=(28, 38, 52))
    for i, t in enumerate(["A-10C II", "AH-64D", "Vector Strike", "DCS-SRS", "F-16C"]):
        y = 70 + i * 34
        if t == "Vector Strike":
            d.rectangle((0, y - 14, 210, y + 16), fill=(52, 70, 92))
        d.text((20, y), t, font=font(12, t == "Vector Strike"), fill=(210, 220, 230), anchor="lm")
    logo = Image.open(os.path.join(THEME, "logo.png")).convert("RGBA").resize((110, 112), Image.LANCZOS)
    img.alpha_composite(logo, (W - 160, 60))
    X, CX = 250, 476
    lab = lambda x, y, s, size=13, col=(205, 215, 225), bold=False: d.text((x, y), s, font=font(size, bold), fill=col, anchor="lm")

    def combo(x, y, val):
        d.rectangle((x, y - 13, x + 225, y + 13), fill=(20, 28, 38), outline=(80, 100, 125))
        lab(x + 8, y, val, 12)
        d.polygon([(x + 210, y - 3), (x + 218, y - 3), (x + 214, y + 3)], fill=(190, 205, 220))

    def check(x, y, s, on=True):
        d.rectangle((x, y - 8, x + 16, y + 8), outline=(140, 160, 185), fill=(20, 28, 38))
        if on:
            d.line((x + 3, y, x + 7, y + 5, x + 13, y - 5), fill=(142, 200, 63), width=3)
        lab(x + 26, y, s, 12)

    lab(X, 80, "Vector Strike -- in-game panel and wheel", 16, (230, 235, 240), True)
    items = []
    y = 140
    lab(X, y, "Show / hide key", 13, (142, 200, 63), True); items.append(y); y += 36
    lab(X, y, "Key"); combo(CX, y, "C"); y += 32
    check(CX, y, "with Ctrl"); check(CX + 165, y, "with Alt"); check(CX + 330, y, "with Shift", False); y += 44
    check(X, y, "Also open the panel with the Comms (radio menu) key"); items.append(y); y += 44
    check(X, y, "Update automatically -- a new version downloads by itself and installs when you close DCS"); items.append(y); y += 44
    lab(X, y, "Panel size"); combo(CX, y, "Auto (fits your screen)"); items.append(y); y += 36
    lab(X, y, "Text size"); combo(CX, y, "100%"); items.append(y); y += 52
    lab(X, y, "Wheel", 13, (142, 200, 63), True); y += 36
    lab(X, y, "Wheel key"); combo(CX, y, "W"); items.append(y); y += 32
    check(CX, y, "with Ctrl"); check(CX + 165, y, "with Alt"); check(CX + 330, y, "with Shift", False); y += 40
    check(X, y, "Point with head tracking (the mouse always works)"); items.append(y); y += 38
    for name, val in [("Head turn to reach an item", "Medium -- 10 degrees"), ("Fire by looking at an item for", "Off -- click or press the key"),
                      ("Wheel size", "100%"), ("Wheel colour", "Vector Strike green"), ("Wheel opacity", "90%"), ("Open the wheel at", "Centre of the screen"),
                      ("Wheel keys (while it is open)", "Arrow keys, Enter, Backspace")]:
        lab(X, y, name); combo(CX, y, val); items.append(y); y += 36
    for i, yy in enumerate(items, start=1):
        callout(img, X - 22, yy, i)
    img.convert("RGB").save(out, quality=90)


def icon_legend(out):
    groups = [
        ("Cargo", [("crate", "Cargo / crate"), ("crate_open", "Unpack"), ("load", "Load"), ("unload", "Unload"),
                   ("spawn", "Spawn crate"), ("destroy", "Destroy crate"), ("radar_sweep", "Nearby crates"), ("list", "List cargo"), ("hangar", "Base supply")]),
        ("Missions", [("troops", "Troops"), ("csar", "CSAR"), ("jtac", "JTAC"), ("target", "Target"), ("smoke", "Smoke"),
                      ("flag", "Objectives"), ("bolt", "Actions"), ("launcher", "Deploy"), ("recon", "Recon")]),
        ("Aircraft & info", [("jet", "Flight"), ("helo", "Helicopter"), ("transport", "C-130"), ("ewr", "EWR / GCI"), ("compass", "Nearest objective"),
                             ("clock", "Takeoff hold"), ("medal", "Points"), ("chat", "Messages"), ("panel", "Open panel")]),
        ("Wheel", [("group", "Your group"), ("more", "More"), ("star", "Pinned"), ("gear", "Wheel editor"), ("wrench", "Repair"),
                   ("fuel", "Fuel"), ("bomb", "Weapons"), ("tank", "Ground"), ("generic", "Other command")]),
    ]
    cols, cell_w, cell_h = 9, 118, 104
    H = 30 + len(groups) * (cell_h + 34)
    img = Image.new("RGBA", (cols * cell_w + 40, H), C["bg"] + (255,))
    d = ImageDraw.Draw(img)
    y = 16
    for title, icons in groups:
        d.text((20, y + 10), title.upper(), font=ImageFont.truetype(FONTS + "DejaVuLGCSansCondensed-Bold.ttf", 15), fill=C["accent"], anchor="lm")
        y += 26
        for i, (name, lab) in enumerate(icons):
            x = 20 + i * cell_w
            ic = vsicons.render(name, 56)
            t = Image.new("RGBA", ic.size, C["text"] + (0,))
            t.putalpha(ic.split()[3])
            img.alpha_composite(t, (x + (cell_w - 56) // 2, y + 4))
            d.text((x + cell_w / 2, y + 76), lab, font=ImageFont.truetype(FONTS + "DejaVuLGCSansCondensed.ttf", 13), fill=C["text"], anchor="mm")
        y += cell_h + 8
    img.convert("RGB").save(out, quality=92)


def main():
    bg_path, out = sys.argv[1], sys.argv[2]
    os.makedirs(out, exist_ok=True)
    bg = backdrop(bg_path, (900, 760))
    for name, fn in [("cockpit-panel-menu", menu_tab), ("cockpit-panel-cargo", cargo_tab), ("cockpit-panel-cargo-category", cargo_category),
                     ("cockpit-panel-status", status_tab), ("cockpit-panel-ewr", ewr_tab), ("cockpit-panel-log", log_tab),
                     ("cockpit-panel-wheel-editor", wheel_tab)]:
        on_background(fn(), bg, scale=0.95).convert("RGB").save(os.path.join(out, name + ".jpeg"), quality=90)
    wheel_image(bg_path, os.path.join(out, "cockpit-wheel-arc.jpeg"), hot_ring=1, arc_hot=3,
                labels=[("crate", "CARGO", True, False), ("troops", "TROOPS", True, False), ("crate", "1/2 CRATES", False, False),
                        ("flag", "OBJECTIVES", True, False), ("compass", "12NM 012°", False, False), ("panel", "F10 MENU", False, False)],
                arc_labels=[("crate_open", "UNPACK", False), ("load", "LOAD", False), ("unload", "UNLOAD", False),
                            ("radar_sweep", "NEARBY", False), ("list", "LIST CARGO", False), ("destroy", "DESTROY", True)],
                hub_title="CARGO › UNLOAD", hub_sub="Cargo > Unload Crate", strip="Unloaded FARP Crate at Hama",
                dot_at=(-100, 296))
    wheel_image(bg_path, os.path.join(out, "cockpit-wheel-info.jpeg"), hot_ring=5, arc_hot=None,
                labels=[("crate", "CARGO", True, False), ("troops", "TROOPS", True, False), ("crate", "1/2 CRATES", False, False),
                        ("flag", "OBJECTIVES", True, False), ("compass", "12NM 012°", False, False), ("panel", "F10 MENU", False, False)],
                arc_labels=None, hub_title="NEAREST OBJECTIVE", hub_sub="Senaki (blue) 11.9 nm @ 012°", dot_at=(150, 150))
    wheel_image(bg_path, os.path.join(out, "cockpit-wheel-blue.jpeg"), hot_ring=2, arc_hot=None,
                labels=[("jtac", "JTAC", True, False), ("ewr", "GCI/EWR", True, False), ("ewr", "EWR PICTURE", False, False),
                        ("compass", "18NM 274°", False, False), ("flag", "OBJECTIVES", True, False), ("bolt", "ACTIONS", True, False),
                        ("jet", "FLIGHT", True, False), ("panel", "F10 MENU", False, False)],
                arc_labels=None, hub_title="GCI/EWR", hub_sub="F10 > GCI/EWR", theme=(79, 163, 224))
    settings_page(os.path.join(out, "cockpit-settings.jpeg"))
    icon_legend(os.path.join(out, "cockpit-icons.jpeg"))
    print("ok")


if __name__ == "__main__":
    main()
