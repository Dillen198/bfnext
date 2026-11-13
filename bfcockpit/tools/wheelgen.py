"""Generate the Vector Strike wheel's textures and layout.

    python bfcockpit/tools/wheelgen.py

Writes bfcockpit/Mods/Services/BFCockpit/Theme/wheel/: white RGBA textures
(DCS tints a picture with its `color`, so one set serves every theme) and
layout.lua, which tells the Hooks script where each texture sits on the
wheel's canvas. Re-run it after changing the geometry or the icon list; the
script and the generated files are committed together.

Icons are the Vector Strike set drawn in vsicons.py, at ICON_PX so they stay
sharp at VR sizes.

Geometry, in canvas units (the canvas is CANVAS square, centred on the hub):
  hub         radius HUB_R
  main ring   R_IN..R_OUT, 2..10 equal segments with GAP degrees between
  outer arc   SUB_IN..SUB_OUT, CHILD_SPAN-degree segments whose start angles
              sit on an ARC_STEP-degree grid, so any arc position is drawn
              from the same 360 / ARC_STEP pre-rendered segments
Angles are screen angles: 0 = right, clockwise, -90 = up.
"""
import math, os, shutil, sys
from PIL import Image, ImageDraw, ImageFilter

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import vsicons

CANVAS = 760
RES = 1.5                    # texture pixels per canvas unit
HUB_R = 92
R_IN, R_OUT, GAP = 104, 232, 2.2
SUB_IN, SUB_OUT = 244, 344
ARC_STEP, CHILD_SPAN, CHILD_GAP = 5, 20, 1.2
GLOW_BLUR = 9
ICON_PX = 96
MAX_RING = 10

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "Mods", "Services", "BFCockpit", "Theme", "wheel")


def sector_points(r_in, r_out, a0, a1, scale, cx, cy):
    pts = []
    steps = max(10, int(abs(a1 - a0) * 3))
    for i in range(steps + 1):
        a = math.radians(a0 + (a1 - a0) * i / steps)
        pts.append((cx + r_out * scale * math.cos(a), cy + r_out * scale * math.sin(a)))
    for i in range(steps, -1, -1):
        a = math.radians(a0 + (a1 - a0) * i / steps)
        pts.append((cx + r_in * scale * math.cos(a), cy + r_in * scale * math.sin(a)))
    return pts


SS = 3  # supersampling on top of RES


def render_layers(draw_fn, pad):
    """Draw on a full supersampled canvas, return (fill, line, glow) images
    and a crop rect in canvas units that holds all three."""
    big = int(CANVAS * RES * SS)
    c = big / 2
    s = RES * SS
    fill = Image.new("L", (big, big), 0)
    line = Image.new("L", (big, big), 0)
    pts = draw_fn(s, c)
    ImageDraw.Draw(fill).polygon(pts, fill=255)
    ImageDraw.Draw(line).line(pts + [pts[0]], fill=255, width=max(1, int(1.6 * s)), joint="curve")
    glow = fill.filter(ImageFilter.GaussianBlur(GLOW_BLUR * s))
    return fill, line, glow


def to_rgba(mask):
    img = Image.new("RGBA", mask.size, (255, 255, 255, 0))
    img.putalpha(mask)
    return img


def save_cropped(layers, name):
    """Crop all three layers to their common bounding box, downsample and
    save; return the rect in canvas units."""
    fill, line, glow = layers
    big = fill.size[0]
    box = glow.getbbox()
    lb = line.getbbox()
    box = (min(box[0], lb[0]), min(box[1], lb[1]), max(box[2], lb[2]), max(box[3], lb[3]))
    # snap to SS so the downsample is exact
    box = tuple(int(math.floor(v / SS) * SS) if i < 2 else int(math.ceil(v / SS) * SS) for i, v in enumerate(box))
    out = {}
    for kind, mask in (("fill", fill), ("line", line), ("glow", glow)):
        crop = mask.crop(box)
        small = crop.resize((crop.size[0] // SS, crop.size[1] // SS), Image.LANCZOS)
        to_rgba(small).save(os.path.join(ROOT, f"{name}_{kind}.png"), optimize=True)
    u = RES * SS
    return {"x": box[0] / u, "y": box[1] / u, "w": (box[2] - box[0]) / u, "h": (box[3] - box[1]) / u}


def lua(v, indent=""):
    if isinstance(v, dict):
        inner = indent + "  "
        parts = []
        for k in sorted(v, key=lambda k: (isinstance(k, str), k)):
            key = f"[{k}]" if isinstance(k, int) else k
            parts.append(f"{inner}{key} = {lua(v[k], inner)},")
        return "{\n" + "\n".join(parts) + "\n" + indent + "}"
    if isinstance(v, list):
        return "{ " + ", ".join(lua(x, indent) for x in v) + " }"
    if isinstance(v, str):
        return '"' + v.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(v, float):
        return f"{v:.2f}"
    return str(v)


def main():
    if os.path.isdir(ROOT):
        shutil.rmtree(ROOT)
    os.makedirs(ROOT)
    layout = {
        "canvas": CANVAS,
        "geom": {"hub_r": HUB_R, "r_in": R_IN, "r_out": R_OUT, "gap": GAP, "sub_in": SUB_IN,
                 "sub_out": SUB_OUT, "arc_step": ARC_STEP, "child_span": CHILD_SPAN, "max_ring": MAX_RING},
        "ring": {}, "arc": {},
    }

    for n in range(2, MAX_RING + 1):
        span = 360 / n
        layout["ring"][n] = {}
        for i in range(n):
            mid = -90 + i * span
            a0, a1 = mid - span / 2 + GAP / 2, mid + span / 2 - GAP / 2
            layers = render_layers(lambda s, c: sector_points(R_IN, R_OUT, a0, a1, s, c, c), GLOW_BLUR)
            layout["ring"][n][i + 1] = save_cropped(layers, f"r{n}_{i + 1}")

    for k in range(360 // ARC_STEP):
        start = k * ARC_STEP
        a0, a1 = start + CHILD_GAP / 2, start + CHILD_SPAN - CHILD_GAP / 2
        layers = render_layers(lambda s, c: sector_points(SUB_IN, SUB_OUT, a0, a1, s, c, c), GLOW_BLUR)
        layout["arc"][k] = save_cropped(layers, f"a{k}")

    # hub: a disc and its rim
    def hub(s, c):
        return [(c + HUB_R * s * math.cos(math.radians(a)), c + HUB_R * s * math.sin(math.radians(a))) for a in range(0, 360, 2)]
    layout["hub"] = save_cropped(render_layers(hub, GLOW_BLUR), "hub")

    # pointer dot (drawn at its own size, centred by the script)
    for name, r in (("dot", 8), ("dot_glow", 18)):
        size = int(r * 2 * RES * SS) + 4 * SS
        m = Image.new("L", (size, size), 0)
        ImageDraw.Draw(m).ellipse((2 * SS, 2 * SS, size - 2 * SS, size - 2 * SS), fill=255)
        if name == "dot_glow":
            m = m.filter(ImageFilter.GaussianBlur(5 * RES * SS))
        to_rgba(m.resize((size // SS, size // SS), Image.LANCZOS)).save(os.path.join(ROOT, f"{name}.png"), optimize=True)
    layout["dot"] = {"r": 8, "glow_r": 18}

    # the answer strip: a rounded rectangle, drawn at the size it is shown
    sw, sh, rad = 640, 46, 12
    for kind in ("fill", "line"):
        m = Image.new("L", (int(sw * RES * SS), int(sh * RES * SS)), 0)
        d = ImageDraw.Draw(m)
        box = (2 * SS, 2 * SS, m.size[0] - 2 * SS, m.size[1] - 2 * SS)
        if kind == "fill":
            d.rounded_rectangle(box, radius=rad * RES * SS, fill=255)
        else:
            d.rounded_rectangle(box, radius=rad * RES * SS, outline=255, width=int(1.4 * RES * SS))
        to_rgba(m.resize((m.size[0] // SS, m.size[1] // SS), Image.LANCZOS)).save(os.path.join(ROOT, f"strip_{kind}.png"), optimize=True)
    layout["strip"] = {"w": sw, "h": sh}

    # icons: the Vector Strike set
    layout["icons"] = vsicons.write_all(os.path.join(ROOT, "icons"), ICON_PX)

    with open(os.path.join(ROOT, "layout.lua"), "w", encoding="utf-8", newline="\r\n") as f:
        f.write("-- Generated by bfcockpit/tools/wheelgen.py. Do not edit by hand.\n")
        f.write("-- Rects are in canvas units; textures are " + str(RES) + "x that in pixels.\n")
        f.write("return " + lua(layout) + "\n")

    total = sum(os.path.getsize(os.path.join(dp, f)) for dp, _, fs in os.walk(ROOT) for f in fs)
    count = sum(len(fs) for _, _, fs in os.walk(ROOT))
    print(f"{count} files, {total / 1024:.0f} KiB in {os.path.normpath(ROOT)}")


if __name__ == "__main__":
    main()
