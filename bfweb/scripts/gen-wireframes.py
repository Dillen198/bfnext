"""Draw the dashboard's wireframe art from the replay's real 3D models.

Each view takes one of public/models/*.glb (the flight replay's models; see
public/models/CREDITS.txt: Tacview's meshes) and keeps the lines a draughtsman would: the
mesh's creases (faces meeting at a sharp angle), its open edges, and its
silhouette from that view's camera. Not every triangle -- on a 6,000-face
model that is a grey smear.

Lines are sorted by what the camera can see: the model's depth is rendered
with a small rasteriser and every edge tested against it. Visible lines are
drawn strong (near brighter than far); lines behind the surface -- the far
side, the cockpit interior, the internal structure a simplified mesh is full
of -- are drawn faint. It keeps the see-through look of a wireframe while
reading like a technical drawing.

Writes public/media/wire/<view>.json: the viewBox size and three path
strings: visible near, visible far, hidden. src/lib/wireframe.ts fetches one when a page
shows it and paints it in the campaign's accent colour.

    python bfweb/scripts/gen-wireframes.py                 # write the views
    python bfweb/scripts/gen-wireframes.py --preview DIR   # and PNGs of them

Needs numpy (and Pillow for --preview).
"""

import json
import math
import struct
import sys
from pathlib import Path

import numpy as np

WEB = Path(__file__).resolve().parents[1]
MODELS = WEB / "public" / "models"
OUT = WEB / "public" / "media" / "wire"

# view: (model, azimuth deg (the eye's bearing from the model: 0 dead ahead,
# 90 starboard), elevation deg, field of view deg, crease angle deg)
VIEWS = {
    # The top bar's: drawn on every page and too small to show hidden lines,
    # so it ships without them (HIDDEN_OFF) -- half the download.
    "carrier": ("Watercraft.CVN-74", 62, 16, 10, 40),
    "carrier-quarter": ("Watercraft.CVN-74", -140, 22, 12, 40),
    "flanker": ("FixedWing.Su-27", 38, 24, 18, 32),
    "flanker-rear": ("FixedWing.Su-27", 215, 14, 18, 32),
    "hornet": ("FixedWing.F-18C", -32, 16, 18, 32),
    "hornet-top": ("FixedWing.F-18C", 60, 62, 18, 32),
    "apache": ("Rotorcraft.AH-64", 30, 12, 20, 36),
    "abrams": ("Vehicle.Tank.M1", 36, 18, 20, 36),
}

HIDDEN_OFF = {"carrier"}

VIEW_H = 200.0
PAD = 4.0


# ── glTF ────────────────────────────────────────────────────────────────────

_COMP = {5120: "b", 5121: "B", 5122: "h", 5123: "H", 5125: "I", 5126: "f"}
_SIZE = {"SCALAR": 1, "VEC2": 2, "VEC3": 3, "VEC4": 4, "MAT4": 16}


def _accessor(gltf, binchunk, idx):
    a = gltf["accessors"][idx]
    bv = gltf["bufferViews"][a["bufferView"]]
    n, comps = a["count"], _SIZE[a["type"]]
    fmt = _COMP[a["componentType"]]
    item = struct.calcsize(fmt)
    stride = bv.get("byteStride") or item * comps
    base = bv.get("byteOffset", 0) + a.get("byteOffset", 0)
    dt = np.dtype("<" + fmt)
    if stride == item * comps:
        arr = np.frombuffer(binchunk, dtype=dt, count=n * comps, offset=base)
        return arr.reshape(n, comps) if comps > 1 else arr
    rows = [np.frombuffer(binchunk, dtype=dt, count=comps, offset=base + i * stride) for i in range(n)]
    return np.array(rows)


def _matrix(node):
    if "matrix" in node:
        return np.array(node["matrix"], dtype=float).reshape(4, 4).T
    t = np.array(node.get("translation", [0, 0, 0]), dtype=float)
    x, y, z, w = node.get("rotation", [0, 0, 0, 1])
    s = np.array(node.get("scale", [1, 1, 1]), dtype=float)
    r = np.array([
        [1 - 2 * (y * y + z * z), 2 * (x * y - z * w), 2 * (x * z + y * w)],
        [2 * (x * y + z * w), 1 - 2 * (x * x + z * z), 2 * (y * z - x * w)],
        [2 * (x * z - y * w), 2 * (y * z + x * w), 1 - 2 * (x * x + y * y)],
    ])
    m = np.eye(4)
    m[:3, :3] = r * s
    m[:3, 3] = t
    return m


def load_glb(path: Path) -> np.ndarray:
    """Every triangle of the model's default scene, (n, 3, 3), in world space."""
    b = path.read_bytes()
    jl = struct.unpack_from("<I", b, 12)[0]
    gltf = json.loads(b[20:20 + jl])
    bl = struct.unpack_from("<I", b, 20 + jl)[0]
    binchunk = b[28 + jl:28 + jl + bl]
    tris = []

    def walk(i, parent):
        node = gltf["nodes"][i]
        m = parent @ _matrix(node)
        if "mesh" in node:
            for prim in gltf["meshes"][node["mesh"]]["primitives"]:
                if prim.get("mode", 4) != 4:
                    continue
                pos = _accessor(gltf, binchunk, prim["attributes"]["POSITION"]).astype(float)
                pos = (np.c_[pos, np.ones(len(pos))] @ m.T)[:, :3]
                idx = (_accessor(gltf, binchunk, prim["indices"]).astype(np.int64)
                       if "indices" in prim else np.arange(len(pos)))
                tris.append(pos[idx.reshape(-1, 3)])
        for c in node.get("children", []):
            walk(c, m)

    scene = gltf["scenes"][gltf.get("scene", 0)]
    for i in scene["nodes"]:
        walk(i, np.eye(4))
    return np.concatenate(tris)


# ── edges ───────────────────────────────────────────────────────────────────

def edge_table(tris: np.ndarray):
    """Welded vertices, the unique edges, and for each edge its faces."""
    size = float(np.ptp(tris.reshape(-1, 3), axis=0).max())
    q = np.round(tris.reshape(-1, 3) / (size * 2e-4)).astype(np.int64)
    _, inv = np.unique(q, axis=0, return_inverse=True)
    inv = inv.reshape(-1, 3)
    verts = np.zeros((inv.max() + 1, 3))
    verts[inv.reshape(-1)] = tris.reshape(-1, 3)
    n = np.cross(tris[:, 1] - tris[:, 0], tris[:, 2] - tris[:, 0])
    ln = np.linalg.norm(n, axis=1)
    ok = ln > size * size * 1e-10
    normals = np.zeros_like(n)
    normals[ok] = n[ok] / ln[ok, None]
    centres = tris.mean(axis=1)
    # Shape quality, 1 for an equilateral triangle, near 0 for a sliver. The
    # replay's simplifier leaves slivers whose sharp angles read as creases:
    # long stray lines across a fuselage.
    sq = (np.linalg.norm(tris[:, 1] - tris[:, 0], axis=1) ** 2 + np.linalg.norm(tris[:, 2] - tris[:, 1], axis=1) ** 2
          + np.linalg.norm(tris[:, 0] - tris[:, 2], axis=1) ** 2)
    quality = np.where(sq > 0, 2 * math.sqrt(3) * ln / np.maximum(sq, 1e-30), 0)
    faces_of: dict[tuple[int, int], list[int]] = {}
    for f, (a, b, c) in enumerate(inv):
        if not ok[f]:
            continue
        for u, v in ((a, b), (b, c), (c, a)):
            if u != v:
                faces_of.setdefault((min(u, v), max(u, v)), []).append(f)
    return verts, faces_of, normals, centres, quality, inv[ok]


SLIVER = 0.15     # face quality below this is a sliver
LONG_CREASE = 0.1  # a crease this long (share of the model's size) beside a sliver is dropped
LONG_OPEN = 0.3    # an open edge this long is a seam the simplifier tore, not an outline


def view_edges(verts, faces_of, normals, centres, quality, eye, crease_deg):
    """Creases, open edges and the silhouette from `eye`."""
    cos_c = math.cos(math.radians(crease_deg))
    size = float(np.ptp(verts, axis=0).max())
    out = []
    for (u, v), fs in faces_of.items():
        length = float(np.linalg.norm(verts[u] - verts[v])) / size
        if len(fs) != 2:
            if len(fs) > 2 or length < LONG_OPEN:
                out.append((u, v))  # open or non-manifold
            continue
        n1, n2 = normals[fs[0]], normals[fs[1]]
        if float(n1 @ n2) < cos_c:
            if not (length > LONG_CREASE and min(quality[fs[0]], quality[fs[1]]) < SLIVER):
                out.append((u, v))
            continue
        s1 = float(n1 @ (eye - centres[fs[0]]))
        s2 = float(n2 @ (eye - centres[fs[1]]))
        if (s1 > 0) != (s2 > 0):
            out.append((u, v))  # silhouette
    return out


def chains(edges):
    """Join edges that meet end to end into polylines, so the SVG repeats
    far fewer coordinates."""
    adj: dict[int, list[int]] = {}
    for u, v in edges:
        adj.setdefault(u, []).append(v)
        adj.setdefault(v, []).append(u)
    used = set()
    lines = []
    order = sorted(adj, key=lambda k: len(adj[k]) != 2)  # start at ends first
    for start in reversed(order):
        for nxt in adj[start]:
            if (min(start, nxt), max(start, nxt)) in used:
                continue
            line = [start]
            cur, prev = nxt, start
            used.add((min(prev, cur), max(prev, cur)))
            line.append(cur)
            while len(adj[cur]) == 2:
                a, b = adj[cur]
                step = b if a == prev else a
                key = (min(cur, step), max(cur, step))
                if key in used:
                    break
                used.add(key)
                prev, cur = cur, step
                line.append(cur)
            lines.append(line)
    return lines


# ── camera ──────────────────────────────────────────────────────────────────

def camera(verts, az, el, fov):
    lo, hi = verts.min(axis=0), verts.max(axis=0)
    target = (lo + hi) / 2
    size = float(np.linalg.norm(hi - lo))
    dist = size / 2 / math.tan(math.radians(fov) / 2)
    a, e = math.radians(az), math.radians(el)
    # Model axes: x to starboard, y the nose, z up (right-handed). Bearing 0
    # is dead ahead (+y), 90 to starboard (+x).
    eye = target + dist * np.array([math.cos(e) * math.sin(a), math.cos(e) * math.cos(a), math.sin(e)])
    f = target - eye
    f /= np.linalg.norm(f)
    r = np.cross(f, [0, 0, 1.0])
    r /= np.linalg.norm(r)
    u = np.cross(r, f)
    return eye, f, r, u


def zbuffer(px, py, z, faces, width, height, k):
    """Depth of the nearest surface per pixel, at k pixels per view unit."""
    W, H = int(math.ceil(width * k)) + 2, int(math.ceil(height * k)) + 2
    zb = np.full((H, W), np.inf)
    X, Y = px * k, py * k
    for a, b, c in faces:
        xs = np.array([X[a], X[b], X[c]])
        ys = np.array([Y[a], Y[b], Y[c]])
        x0, x1 = max(int(xs.min()), 0), min(int(xs.max()) + 1, W - 1)
        y0, y1 = max(int(ys.min()), 0), min(int(ys.max()) + 1, H - 1)
        if x1 < x0 or y1 < y0:
            continue
        gx, gy = np.meshgrid(np.arange(x0, x1 + 1) + 0.5, np.arange(y0, y1 + 1) + 0.5)
        d = (ys[1] - ys[2]) * (xs[0] - xs[2]) + (xs[2] - xs[1]) * (ys[0] - ys[2])
        if abs(d) < 1e-12:
            continue
        l0 = ((ys[1] - ys[2]) * (gx - xs[2]) + (xs[2] - xs[1]) * (gy - ys[2])) / d
        l1 = ((ys[2] - ys[0]) * (gx - xs[2]) + (xs[0] - xs[2]) * (gy - ys[2])) / d
        l2 = 1 - l0 - l1
        inside = (l0 >= -1e-6) & (l1 >= -1e-6) & (l2 >= -1e-6)
        if not inside.any():
            continue
        depth = 1 / (l0 / z[a] + l1 / z[b] + l2 / z[c])  # perspective-correct
        sub = zb[y0:y1 + 1, x0:x1 + 1]
        np.minimum(sub, np.where(inside, depth, np.inf), out=sub)
    # The farthest of each pixel's neighbours: an edge lies on its own faces,
    # and on a silhouette half its pixels are the surface it outlines -- it
    # counts as seen if any pixel beside it shows it.
    far = zb.copy()
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            np.maximum(far, np.roll(np.roll(zb, dy, 0), dx, 1), out=far)
    return far


def visibility(a, b, px, py, z, zb, k, eps):
    """Visible (True) / hidden (False) runs along edge a-b, as (t0, t1, vis)."""
    L = math.hypot(px[b] - px[a], py[b] - py[a]) * k
    n = max(2, int(L) + 1)
    t = np.linspace(0, 1, n)
    x = (px[a] + (px[b] - px[a]) * t) * k
    y = (py[a] + (py[b] - py[a]) * t) * k
    depth = 1 / ((1 - t) / z[a] + t / z[b])
    xi = np.clip(x.astype(int), 0, zb.shape[1] - 1)
    yi = np.clip(y.astype(int), 0, zb.shape[0] - 1)
    vis = depth <= zb[yi, xi] + eps
    runs, start = [], 0
    for i in range(1, n + 1):
        if i == n or vis[i] != vis[start]:
            runs.append((t[start], t[i - 1], bool(vis[start])))
            start = i
    return runs


def render(name):
    model, az, el, fov, crease = VIEWS[name]
    tris = load_glb(MODELS / f"{model}.glb")
    verts, faces_of, normals, centres, quality, faces = edge_table(tris)
    eye, f, r, u = camera(verts, az, el, fov)
    edges = view_edges(verts, faces_of, normals, centres, quality, eye, crease)
    d = verts - eye
    z = d @ f
    sx, sy = (d @ r) / z, -(d @ u) / z
    used = np.unique(np.array(edges).reshape(-1))
    x0, x1, y0, y1 = sx[used].min(), sx[used].max(), sy[used].min(), sy[used].max()
    scale = (VIEW_H - 2 * PAD) / (y1 - y0)
    px, py = (sx - x0) * scale + PAD, (sy - y0) * scale + PAD
    width = (x1 - x0) * scale + 2 * PAD
    # Sub-pixel detail (antenna farms, cockpit switches) is noise at banner
    # size and most of the file: drop segments shorter than half a unit.
    edges = [(a, b) for (a, b) in edges if math.hypot(px[a] - px[b], py[a] - py[b]) >= 0.5]
    zn, zf = z[used].min(), z[used].max()
    k = 4  # depth-buffer pixels per view unit
    zb = zbuffer(px, py, z, faces, width, VIEW_H, k)
    eps = 0.01 * (zf - zn)
    near_cut = zn + 0.45 * (zf - zn)
    whole = [[], [], []]   # edges entirely in one class: chained
    parts = [[], [], []]   # pieces of edges that change class part-way
    for (a, b) in edges:
        runs = visibility(a, b, px, py, z, zb, k, eps)
        mid_z = (z[a] + z[b]) / 2

        def cls(vis):
            return (0 if mid_z < near_cut else 1) if vis else 2
        if len(runs) == 1:
            whole[cls(runs[0][2])].append((a, b))
            continue
        for t0, t1, vis in runs:
            if t1 <= t0:
                continue
            parts[cls(vis)].append(((px[a] + (px[b] - px[a]) * t0, py[a] + (py[b] - py[a]) * t0),
                                    (px[a] + (px[b] - px[a]) * t1, py[a] + (py[b] - py[a]) * t1)))
    paths = []
    for i in range(3):
        s_ = []
        for line in chains(whole[i]):
            s_.append("M" + "L".join(f"{px[j]:.1f} {py[j]:.1f}" for j in line))
        for (x0_, y0_), (x1_, y1_) in parts[i]:
            s_.append(f"M{x0_:.1f} {y0_:.1f}L{x1_:.1f} {y1_:.1f}")
        paths.append("".join(s_))
    if name in HIDDEN_OFF:
        paths[2] = ""
    return {"w": round(width), "h": round(VIEW_H), "paths": paths}, len(edges)


def preview(directory: str, views):
    from PIL import Image, ImageDraw
    d = Path(directory)
    d.mkdir(parents=True, exist_ok=True)
    for name, art in views.items():
        k = 3
        img = Image.new("RGB", (int(art["w"] * k), int(art["h"] * k)), (12, 16, 10))
        dr = ImageDraw.Draw(img)
        for path, col in zip(art["paths"], ((150, 220, 80), (100, 150, 58), (34, 48, 24))):
            for seg in path.split("M")[1:]:
                pts = [tuple(float(v) * k for v in pt.split()) for pt in seg.split("L")]
                if len(pts) > 1:
                    dr.line(pts, fill=col, width=1)
        img.save(d / f"{name}.png")
    print(f"previews in {d}")


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    views = {}
    for name in VIEWS:
        art, n = render(name)
        views[name] = art
        (OUT / f"{name}.json").write_text(json.dumps(art, separators=(",", ":")), encoding="utf-8")
        size = (OUT / f"{name}.json").stat().st_size
        print(f"{name}: {art['w']}x{art['h']}, {n} edges, {size // 1024} KiB")
    if len(sys.argv) > 2 and sys.argv[1] == "--preview":
        preview(sys.argv[2], views)


if __name__ == "__main__":
    main()
