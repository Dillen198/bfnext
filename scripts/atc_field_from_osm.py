#!/usr/bin/env python3
"""Build an airfield file for the range's spoken ATC (bfdb/src/milatc) from
OpenStreetMap.

DCS exposes runways (`Airbase:getRunways()`) and parking, but not the taxiway
network, so the taxi routes, taxiway names and hold-short points that Ground and
Tower speak from come from OSM instead. DCS's newer terrains (Marianas
included) are built from real survey data, so the two line up to within a few
tens of metres. bfdb measures the remaining offset against the DCS runway
thresholds when it starts and shifts the network to match.

    python scripts/atc_field_from_osm.py --icao PGUA --name "Andersen AFB" \
        --callsign Andersen --bbox 13.550,144.880,13.610,144.965 \
        --out bfdb/data/atc/andersen.json

Pass --osm FILE to build from a saved Overpass reply instead of fetching one.
The output is meant to be committed and then hand-tuned: ramp names and the
default frequencies are guesses, so check them.
"""

import argparse
import json
import math
import sys
import urllib.parse
import urllib.request

OVERPASS = "https://overpass-api.de/api/interpreter"

# How far a hold-short line sits from the runway centreline. 250 ft (76 m) is
# the usual figure for a runway that takes heavy jets.
HOLD_M = 76.0


def fetch(bbox: str) -> dict:
    s, w, n, e = bbox.split(",")
    box = f"({s},{w},{n},{e})"
    q = (
        "[out:json][timeout:60];("
        f'way["aeroway"~"^(taxiway|taxilane|runway|apron)$"]{box};'
        f'node["aeroway"~"^(holding_position|parking_position)$"]{box};'
        ");out body geom;"
    )
    req = urllib.request.Request(
        OVERPASS + "?data=" + urllib.parse.quote(q),
        headers={"User-Agent": "bfnext-atc-field-builder/1.0"},
    )
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.load(r)


class Local:
    """Flat-earth metres around a reference point: x north, y east."""

    def __init__(self, lat0: float, lon0: float):
        self.lat0, self.lon0 = lat0, lon0
        self.k_lat = 111_132.0
        self.k_lon = 111_320.0 * math.cos(math.radians(lat0))

    def xy(self, lat, lon):
        return ((lat - self.lat0) * self.k_lat, (lon - self.lon0) * self.k_lon)

    def ll(self, x, y):
        return (self.lat0 + x / self.k_lat, self.lon0 + y / self.k_lon)


def norm_rwy(r: str) -> str:
    """'6R' -> '06R'."""
    num = "".join(c for c in r if c.isdigit())
    suf = "".join(c for c in r if c.isalpha()).upper()
    return f"{int(num):02d}{suf}"


def heading(ax, ay, bx, by) -> float:
    return math.degrees(math.atan2(by - ay, bx - ax)) % 360.0


def build(osm: dict, icao: str, name: str, callsign: str) -> dict:
    els = osm["elements"]
    lats = [p["lat"] for e in els for p in (e.get("geometry") or [])]
    lons = [p["lon"] for e in els for p in (e.get("geometry") or [])]
    if not lats:
        sys.exit("no aeroway geometry in the OSM data")
    loc = Local(sum(lats) / len(lats), sum(lons) / len(lons))

    # ── runways ──────────────────────────────────────────────────────────
    runways = []  # (pair name, end names, a xy, b xy, width)
    for e in els:
        t = e.get("tags", {})
        if e["type"] != "way" or t.get("aeroway") != "runway":
            continue
        ref = t.get("ref", "")
        ends = [norm_rwy(x) for x in ref.split("/") if x.strip()]
        g = e["geometry"]
        a = loc.xy(g[0]["lat"], g[0]["lon"])
        b = loc.xy(g[-1]["lat"], g[-1]["lon"])
        width = float(t.get("width", "45").split()[0]) if t.get("width") else 45.0
        if len(ends) != 2:
            continue
        # Which end is which: the end you land on heading H is the one you
        # approach from, so "06" is the threshold at the *west* end of an
        # 060/240 strip. Match by heading.
        h_ab = heading(*a, *b)
        n0 = int("".join(c for c in ends[0] if c.isdigit())) * 10
        d = abs(((h_ab - n0) + 540) % 360 - 180)
        if d > 90:  # ends[0] lands the other way: swap the geometry
            a, b = b, a
        runways.append(("/".join(ends), ends, a, b, width))

    out_rwys = []
    for pair, ends, a, b, width in runways:
        length = math.dist(a, b)
        for end, thr, far in ((ends[0], a, b), (ends[1], b, a)):
            lat, lon = loc.ll(*thr)
            out_rwys.append(
                {
                    "name": end,
                    "pair": pair,
                    "thrLat": round(lat, 7),
                    "thrLon": round(lon, 7),
                    "hdgTrue": round(heading(*thr, *far), 1),
                    "lengthM": round(length),
                    "widthM": round(width),
                }
            )

    def rwy_frame(xy):
        """Nearest runway to a point: (pair, along from ends[0], cross-track)."""
        best = None
        for pair, _ends, a, b, _w in runways:
            ux, uy = b[0] - a[0], b[1] - a[1]
            L = math.hypot(ux, uy)
            ux, uy = ux / L, uy / L
            dx, dy = xy[0] - a[0], xy[1] - a[1]
            along = dx * ux + dy * uy
            cross = -dx * uy + dy * ux
            if -150 <= along <= L + 150:
                if best is None or abs(cross) < abs(best[2]):
                    best = (pair, along, cross, L)
        return best

    # ── taxi network ─────────────────────────────────────────────────────
    nodes = {}  # osm id -> xy
    edges = []  # (a, b, name)
    for e in els:
        t = e.get("tags", {})
        if e["type"] != "way" or t.get("aeroway") not in ("taxiway", "taxilane"):
            continue
        ids = e.get("nodes") or []
        geom = e.get("geometry") or []
        if len(ids) != len(geom) or len(ids) < 2:
            continue
        ref = (t.get("ref") or "").strip().upper()
        for nid, p in zip(ids, geom):
            nodes[nid] = loc.xy(p["lat"], p["lon"])
        for x, y in zip(ids, ids[1:]):
            if x != y:
                edges.append((x, y, ref))

    # Split every edge that crosses a hold line and record the hold point.
    # An edge crossing the line twice (straight across a runway) gets both.
    next_id = -1
    holds = []
    split_edges = []
    for a, b, ref in edges:
        pa, pb = nodes[a], nodes[b]
        fa, fb = rwy_frame(pa), rwy_frame(pb)
        cuts = []
        for f0, f1 in ((fa, fb),):
            if not f0 and not f1:
                continue
            pair = (f0 or f1)[0]
            if f0 and f1 and f0[0] != f1[0]:
                continue
            c0 = f0[2] if f0 else 1e9
            c1 = f1[2] if f1 else 1e9
            for line in (HOLD_M, -HOLD_M):
                if (c0 - line) * (c1 - line) < 0 and abs(c0) < 1e8 and abs(c1) < 1e8:
                    s = (line - c0) / (c1 - c0)
                    cuts.append((s, pair, line))
        if not cuts:
            split_edges.append((a, b, ref))
            continue
        cuts.sort()
        prev = a
        for s, pair, line in cuts:
            nid = next_id
            next_id -= 1
            nodes[nid] = (pa[0] + (pb[0] - pa[0]) * s, pa[1] + (pb[1] - pa[1]) * s)
            split_edges.append((prev, nid, ref))
            f = rwy_frame(nodes[nid])
            holds.append({"node": nid, "runway": pair, "taxiway": ref, "along": f[1], "len": f[3]})
            prev = nid
        split_edges.append((prev, b, ref))

    # Mark edges on or across a runway, so the router can avoid crossings and
    # Ground can say "cross runway ...".
    def on_rwy(xy):
        f = rwy_frame(xy)
        return f[0] if f and abs(f[2]) < HOLD_M - 1 else None

    # Compact ids, drop nodes no edge uses.
    used = sorted({n for a, b, _ in split_edges for n in (a, b)}, key=lambda n: (n < 0, abs(n)))
    idx = {n: i for i, n in enumerate(used)}
    out_nodes = []
    for n in used:
        lat, lon = loc.ll(*nodes[n])
        out_nodes.append([round(lat, 7), round(lon, 7)])
    out_edges = []
    for a, b, ref in split_edges:
        mid = ((nodes[a][0] + nodes[b][0]) / 2, (nodes[a][1] + nodes[b][1]) / 2)
        r = on_rwy(mid)
        e = {"a": idx[a], "b": idx[b], "name": ref}
        if r:
            e["runway"] = r
        out_edges.append(e)

    # Name each hold by the threshold it is nearest, so a departure from 06R
    # can ask for "the holds serving 06R".
    out_holds = []
    for h in holds:
        pair = h["runway"]
        ends = pair.split("/")
        near_end = ends[0] if h["along"] < h["len"] / 2 else ends[1]
        dist_thr = h["along"] if near_end == ends[0] else h["len"] - h["along"]
        out_holds.append(
            {
                "node": idx[h["node"]],
                "runway": pair,
                "taxiway": h["taxiway"],
                "nearEnd": near_end,
                "fromThrM": round(dist_thr),
            }
        )

    # ── ramps ────────────────────────────────────────────────────────────
    cx = sum(p[0] for p in nodes.values()) / len(nodes)
    cy = sum(p[1] for p in nodes.values()) / len(nodes)
    ramps = []
    for e in els:
        t = e.get("tags", {})
        if e["type"] != "way" or t.get("aeroway") != "apron":
            continue
        g = e["geometry"]
        pts = [loc.xy(p["lat"], p["lon"]) for p in g]
        mx = sum(p[0] for p in pts) / len(pts)
        my = sum(p[1] for p in pts) / len(pts)
        area = 0.0
        for (x0, y0), (x1, y1) in zip(pts, pts[1:] + pts[:1]):
            area += x0 * y1 - x1 * y0
        area = abs(area) / 2
        if area < 4000:  # a pad, not a ramp
            continue
        compass = ["north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest"]
        b = heading(cx, cy, mx, my)
        lat, lon = loc.ll(mx, my)
        ramps.append(
            {
                "name": t.get("name") or f"{compass[int((b + 22.5) // 45) % 8]} ramp",
                "lat": round(lat, 7),
                "lon": round(lon, 7),
                "areaM2": round(area),
            }
        )
    ramps.sort(key=lambda r: -r["areaM2"])
    seen = {}
    for r in ramps:
        k = r["name"]
        seen[k] = seen.get(k, 0) + 1
        if seen[k] > 1:
            r["name"] = f"{k} {seen[k]}"

    return {
        "icao": icao,
        "dcsName": name,
        "callsign": callsign,
        "source": "OpenStreetMap contributors (ODbL), aeroway=taxiway/runway/apron",
        "runways": out_rwys,
        "nodes": out_nodes,
        "edges": out_edges,
        "holds": out_holds,
        "ramps": ramps,
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--icao", required=True)
    ap.add_argument("--name", required=True, help="the airbase name as DCS has it")
    ap.add_argument("--callsign", required=True, help='what controllers call themselves ("Andersen")')
    ap.add_argument("--bbox", help="south,west,north,east")
    ap.add_argument("--osm", help="a saved Overpass JSON reply")
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    if a.osm:
        with open(a.osm, encoding="utf-8") as f:
            osm = json.load(f)
    elif a.bbox:
        osm = fetch(a.bbox)
    else:
        sys.exit("give --bbox or --osm")
    field = build(osm, a.icao, a.name, a.callsign)
    # Keep any hand-tuned sections of an existing file.
    try:
        with open(a.out, encoding="utf-8") as f:
            old = json.load(f)
        for k in ("positions", "runwayUse", "departure", "arrival", "ramps"):
            if k in old:
                field[k] = old[k]
    except (OSError, ValueError):
        pass
    with open(a.out, "w", encoding="utf-8") as f:
        json.dump(field, f, indent=1)
        f.write("\n")
    print(
        f"{a.out}: {len(field['runways'])} runway ends, {len(field['nodes'])} nodes, "
        f"{len(field['edges'])} edges, {len(field['holds'])} hold points, {len(field['ramps'])} ramps"
    )


if __name__ == "__main__":
    main()
