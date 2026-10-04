# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""The training sectors beyond the first range: live SEAD/IADS ranges,
tiered air-to-ground, EW/GPS jamming, hot zones with an AWACS, low-level
routes, field landing grading, CSAR, frigate decks and escorted shipping.

layout.py says WHERE (sectors, zones and the tables IADS, TIERED, JAMMERS,
HOT_ZONES, ROUTES, CSAR_AREAS, PATTERN_FIELDS, DECKS, ESCORTED); this module
turns those tables into the range config's sections and into the extra F10
drawings (threat rings, jammer radii, route lines) the builder adds to the
sector outlines.
"""
import io
import math
import struct
import wave

import layout
from layout import NM

# Engagement ranges, km, for the F10 threat rings. The engine's own table
# (bfrange/src/iads.rs SYSTEMS) is what it fights with; keep them in step.
SYSTEM_RANGE_KM = {
    "sa2": 43, "sa3": 18, "sa5": 150, "sa6": 25, "sa8": 10, "sa10": 75, "sa11": 35, "sa13": 5,
    "sa15": 12, "sa19": 8, "pantsir": 20, "tor_m2": 15, "zsu": 2.5, "ewr_1l13": 0, "ewr_55g6": 0,
    "hawk": 45, "patriot": 100, "nasams": 25, "iris_t": 30, "roland": 8, "gepard": 4, "avenger": 5,
    "rapier": 7, "hq7": 12, "ewr_fps117": 0,
}
COUNTRY = {"red": "CJTF Red", "blue": "CJTF Blue"}


def cfg_loc(zone):
    return {"zone": zone}


def cfg_ll(ll):
    return {"lat": round(ll[0], 5), "lon": round(ll[1], 5)}


def _sector(sid):
    return next(s for s in layout.SECTORS if s["id"] == sid)


def _circle_of(sid):
    """(centre, radius_m) that covers a sector, for engage areas."""
    sh = _sector(sid)["shape"]
    if sh[0] == "circle":
        return sh[1], sh[2]
    if sh[0] == "rect":
        return sh[1], math.hypot(sh[2], sh[3]) / 2
    raise ValueError(sh[0])


# ------------------------------------------------------------------ config

def iads_cfg():
    out = []
    for n in layout.IADS:
        c, r = _circle_of(n["sector"])
        out.append({
            "id": n["id"], "name": n["name"], "side": n["side"], "country": COUNTRY[n["side"]],
            "emcon": n.get("emcon", "iads"), "harm_defence_s": n.get("harm_defence_s", 45),
            "weapons_free": True, "respawn_s": n.get("respawn_s", 900),
            # the sector plus a few miles: nobody outside it wakes the network
            "engage_within": {"loc": cfg_ll(c), "radius_nm": round(r / NM + 5, 1)},
            "sites": [{"id": z.lower(), "name": name, "system": system, "loc": cfg_loc(z), "heading_deg": hdg}
                      for z, system, name, hdg in n["sites"]],
        })
    return out


# Tiered target kit: (type, category, x, y). Red kit for blue pilots.
TIER_KIT = {
    "red": {
        "easy": [("Ural-375", 0, 0), ("Ural-375", 40, 25), ("GAZ-66", -35, 30), ("KAMAZ Truck", 30, -40), ("Ural-375", -50, -25)],
        "medium": [("BMP-2", 0, 0), ("BMP-2", 45, 25), ("BTR-80", -40, 30), ("T-72B", 35, -35),
                   ("ZSU-23-4 Shilka", 140, 0), ("Ural-375 ZU-23", -130, 60)],
        "hard": [("CHAP_T90M", 0, 0), ("CHAP_T90M", 45, 25), ("CHAP_BMPT", -40, 30), ("BMP-3", 35, -35),
                 ("2S6 Tunguska", 220, 40), ("Strela-10M3", -200, -60), ("CHAP_PantsirS1", 60, -260)],
    },
    "blue": {
        "easy": [("M 818", 0, 0), ("M 818", 40, 25), ("Hummer", -35, 30), ("M 818", 30, -40), ("Hummer", -50, -25)],
        "medium": [("M-2 Bradley", 0, 0), ("M-2 Bradley", 45, 25), ("LAV-25", -40, 30), ("M-1 Abrams", 35, -35),
                   ("Gepard", 140, 0), ("Vulcan", -130, 60)],
        "hard": [("Leopard-2", 0, 0), ("Leopard-2", 45, 25), ("Marder", -40, 30), ("CHAP_M1130", 35, -35),
                 ("M6 Linebacker", 220, 40), ("M1097 Avenger", -200, -60), ("Roland ADS", 60, -260)],
    },
}
TIER_NOTE = {
    "easy": "Trucks, nothing shoots back. Learn the weapon.",
    "medium": "APCs and a tank with AAA covering them: the guns are real, stay above 10,000 ft or come in fast.",
    "hard": "Modern armour under SHORAD (SA-19, SA-13, Pantsir / Linebacker, Avenger, Roland): weapons free, the missile trainer protects you.",
}


def tiered_stations():
    out = []
    for t in layout.TIERED:
        kit_side = t["targets"]
        for tier in ("easy", "medium", "hard"):
            kit = TIER_KIT[kit_side][tier]
            out.append({
                "id": f"{t['sid']}-{tier}", "name": f"{t['name']} - {tier.capitalize()}", "kind": "tactical_array",
                "loc": cfg_loc(f"{t['prefix']}-{tier.upper()}"), "heading_deg": 45, "rings_m": [25, 50],
                "side": kit_side, "country": COUNTRY[kit_side], "respawn_s": 300, "tier": tier,
                "weapons_free": tier != "easy",
                "targets": [{"typ": typ, "category": "vehicle", "x": x, "y": y} for typ, x, y in kit],
                "note": TIER_NOTE[tier],
            })
    return out


def jammer_cfg_and_stations():
    jams, stations = [], []
    for j in layout.JAMMERS:
        target_side = j["side"]
        jams.append({
            "id": j["id"], "name": j["name"], "side": target_side, "country": COUNTRY[target_side],
            "loc": cfg_loc(j["zone"]), "gps": j.get("gps", "jam"), "glonass": j.get("glonass", "jam"),
            "radio": j.get("radio", "off"), "radius_nm": j["radius_nm"],
        })
        stations.append({
            "id": f"{j['id']}-targets", "name": f"{j['name']} - GPS-denied targets", "kind": "coord_target",
            "loc": cfg_loc(j["targets_zone"]), "side": target_side, "country": COUNTRY[target_side], "respawn_s": 900,
            "targets": [{"typ": "Hangar A", "category": "static", "name": "Hangar"},
                        {"typ": "Garage A", "category": "static", "x": 120, "y": 60, "name": "Garage"},
                        {"typ": ".Command Center", "category": "static", "x": -150, "y": 80, "name": "Command post"}],
            "note": "Inside the jammer's radius: GPS/INS weapons degrade. Kill the jammer, or use laser, TV or your eyes.",
        })
    return jams, stations


def hot_zones_cfg():
    out = []
    for h in layout.HOT_ZONES:
        sh = _sector(h["sector"])["shape"]
        assert sh[0] == "circle", h["sector"]
        a = h["awacs"]
        out.append({
            "id": h["id"], "name": h["name"], "loc": cfg_ll(sh[1]), "radius_nm": round(sh[2] / NM, 1),
            "ai_side": h["ai_side"], "country": COUNTRY[h["ai_side"]],
            "cap_types": h["cap_types"], "cap_flights": h.get("cap_flights", 2), "flight_size": 2,
            "cap_skill": h.get("cap_skill", "Good"), "cap_weapons": "fox3", "cap_respawn_s": 300,
            "cap_alt_ft": 22000, "idle_despawn_s": 600,
            "ground": [{"id": z.lower(), "name": name, "loc": cfg_loc(z), "composition": comp, "respawn_s": 600}
                       for z, comp, name in h["sites"]],
            "awacs": {"typ": a["typ"], "callsign": a["callsign"], "callsign_number": a.get("n", 1),
                      "loc": cfg_ll(a["at"]), "heading_deg": a["heading"], "leg_nm": 25, "alt_ft": 30000,
                      "freq_mhz": a["freq"]},
        })
    return out


def routes_cfg():
    out = []
    for r in layout.ROUTES:
        out.append({
            "id": r["id"], "name": r["name"], "side": r["side"],
            "gates": [{"name": name, "loc": cfg_loc(z), "radius_m": r.get("gate_m", 1000)} for z, name in r["gates"]],
            "max_agl_ft": r.get("max_agl_ft", 500), "min_agl_ft": r.get("min_agl_ft", 100),
            "speed_kts": r.get("speed_kts", 420), "tot_tolerance_s": 15,
        })
    return out


BEACON_FILE = "l10n/DEFAULT/csar_beacon.wav"


def csar_cfg():
    return {
        "beacon_file": BEACON_FILE,
        "areas": [{"id": c["id"], "name": c["name"], "side": c["side"], "loc": cfg_ll(_sector(c["sector"])["shape"][1]),
                   "radius_m": round(_sector(c["sector"])["shape"][2] * 0.95, 1), "beacon_khz": c["khz"]}
                  for c in layout.CSAR_AREAS],
    }


def pattern_cfg():
    # every airfield is graded; the pattern sectors are just where to go
    # for circuits away from the busiest fields
    return {"enabled": True, "fields": [], "glideslope_deg": 3.0, "aim_point_m": 300}


def decks_cfg():
    out = []
    for d in layout.DECKS:
        e = {"id": d["id"], "name": d["name"], "typ": d["typ"], "side": d["side"], "country": COUNTRY[d["side"]],
             "loc": cfg_ll(d["at"]), "heading_deg": d["heading"], "speed_kts": d.get("speed_kts", 10),
             "leg_nm": d.get("leg_nm", 6), "perfect_m": 1.5}
        if "tacan" in d:
            ch, band, morse = d["tacan"]
            e["tacan"] = {"channel": ch, "band": band, "morse": morse}
        out.append(e)
    return out


def escorted_stations():
    out = []
    for e in layout.ESCORTED:
        side = e["targets"]
        out.append({
            "id": e["id"], "name": e["name"], "kind": "ship_target", "loc": cfg_ll(e["start"]),
            "side": side, "country": COUNTRY[side], "respawn_s": 1200, "weapons_free": True,
            "targets": [{"typ": typ, "category": "ship", "x": x, "y": y} for typ, x, y in e["ships"]],
            "route": {"points": [cfg_ll(e["end"])], "speed_kts": 14},
            "note": e["note"],
        })
    return out


def extra_stations():
    _, jam_stations = jammer_cfg_and_stations()
    return tiered_stations() + jam_stations + escorted_stations()


def config_sections():
    jams, _ = jammer_cfg_and_stations()
    return {
        "iads": iads_cfg(),
        "hot_zones": hot_zones_cfg(),
        "jammers": jams,
        "low_level": routes_cfg(),
        "pattern": pattern_cfg(),
        "csar": csar_cfg(),
        "ship_decks": decks_cfg(),
    }


# ------------------------------------------------------------------ drawings

def _rgba(rgb, alpha):
    return "0x%06x%02x" % (rgb, alpha)


def extra_drawings(ll2xz):
    """Threat rings round every IADS site, jammer radii, and each low-level
    route as a line with its gates. Returns {layer: [objects]}."""
    layers = {"Blue": [], "Red": [], "Common": []}
    attacker_layer = {"red": "Blue", "blue": "Red"}
    zones = {z[0]: z[1] for z in layout.ZONES}
    for n in layout.IADS:
        layer = attacker_layer[n["side"]]
        for z, system, name, _ in n["sites"]:
            x, y = ll2xz(*zones[z])
            r = SYSTEM_RANGE_KM[system] * 1000
            if r > 0:
                layers[layer].append({"visible": True, "layerName": layer, "primitiveType": "Polygon",
                                      "polygonMode": "circle", "radius": r, "mapX": x, "mapY": y,
                                      "name": f"{z} ring", "colorString": _rgba(0xFF3B3B, 0xc0),
                                      "fillColorString": _rgba(0xFF3B3B, 0x00), "thickness": 2,
                                      "style": "dash", "angle": 0})
    for j in layout.JAMMERS:
        layer = attacker_layer[j["side"]]
        x, y = ll2xz(*zones[j["zone"]])
        layers[layer].append({"visible": True, "layerName": layer, "primitiveType": "Polygon",
                              "polygonMode": "circle", "radius": j["radius_nm"] * NM, "mapX": x, "mapY": y,
                              "name": f"{j['id']} radius", "colorString": _rgba(layout.COLOURS["ew"], 0xff),
                              "fillColorString": _rgba(layout.COLOURS["ew"], 0x14), "thickness": 3,
                              "style": "dash", "angle": 0})
    for r in layout.ROUTES:
        layer = {"blue": "Blue", "red": "Red", "all": "Common"}[r["side"]]
        pts = [ll2xz(*zones[z]) for z, _ in r["gates"]]
        x0, y0 = pts[0]
        layers[layer].append({"visible": True, "layerName": layer, "primitiveType": "Line", "lineMode": "segments",
                              "name": f"{r['id']} route", "mapX": x0, "mapY": y0, "closed": False,
                              "colorString": _rgba(layout.COLOURS["low_level"], 0xff), "thickness": 4,
                              "style": "dash", "points": [{"x": px - x0, "y": py - y0} for px, py in pts]})
        # the sector's own label sits at gate 1 and says where it starts
        for (z, _), (px, py) in zip(r["gates"], pts):
            layers[layer].append({"visible": True, "layerName": layer, "primitiveType": "Polygon",
                                  "polygonMode": "circle", "radius": r.get("gate_m", 1000), "mapX": px, "mapY": py,
                                  "name": f"{z} gate", "colorString": _rgba(layout.COLOURS["low_level"], 0xff),
                                  "fillColorString": _rgba(layout.COLOURS["low_level"], 0x30), "thickness": 3,
                                  "style": "solid", "angle": 0})
    return layers


# ------------------------------------------------------------------ beacon

def beacon_wav():
    """A CSAR beacon: a 1 kHz tone keyed on and off, 8 kHz mono PCM. DCS
    plays .wav from the mission file for trigger.action.radioTransmission."""
    rate = 8000
    buf = io.BytesIO()
    with wave.open(buf, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(rate)
        frames = bytearray()
        for i in range(rate * 3):
            t = i / rate
            on = (t % 1.0) < 0.35 or 1.5 <= (t % 3.0) < 1.85
            v = int(12000 * math.sin(2 * math.pi * 1000 * t)) if on else 0
            frames += struct.pack("<h", v)
        w.writeframes(bytes(frames))
    return buf.getvalue()
