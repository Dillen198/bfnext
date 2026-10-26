"""Filter OurAirports (public domain, github.com/davidmegginson/ourairports-data)
to the DCS theatres: airfields with a usable runway, as compact JSON for the
replay's F10-style map (bfweb/src/replay/dcs/airfields.json).

usage: python gen-dcs-airfields.py airports.csv runways.csv <out.json>"""
import csv, json, sys

airports_csv, runways_csv, out = sys.argv[1:4]

# Rough lat/lon boxes of the DCS theatres: (name, south, north, west, east)
THEATRES = [
    ('Caucasus', 40.5, 46.2, 36.5, 46.6),
    ('Syria', 31.8, 37.6, 31.8, 40.8),
    ('PersianGulf', 23.0, 30.6, 50.5, 59.8),
    ('Marianas', 12.5, 21.5, 143.0, 147.5),
    ('Nevada', 34.9, 38.6, -117.6, -113.9),
    ('Normandy', 48.0, 51.6, -3.6, 1.8),
    ('TheChannel', 50.0, 51.9, -0.5, 3.6),
    ('Sinai', 27.4, 32.6, 29.4, 35.6),
    ('SouthAtlantic', -56.5, -48.5, -77.0, -53.0),
    ('Kola', 64.0, 71.2, 13.5, 38.5),
    ('Afghanistan', 29.0, 38.6, 59.8, 72.5),
    ('Iraq', 29.0, 37.6, 38.5, 48.8),
    ('GermanyCW', 49.3, 55.2, 5.8, 16.2),
]

def theatre(lat, lon):
    for name, s, n, w, e in THEATRES:
        if s <= lat <= n and w <= lon <= e:
            return name
    return None

HARD = ('ASP', 'CON', 'PEM', 'BIT', 'TAR', 'MAC', 'ASF')

airports = {}
with open(airports_csv, encoding='utf-8') as f:
    for r in csv.DictReader(f):
        if r['type'] not in ('large_airport', 'medium_airport', 'small_airport'):
            continue
        try:
            lat, lon = float(r['latitude_deg']), float(r['longitude_deg'])
        except ValueError:
            continue
        th = theatre(lat, lon)
        if not th:
            continue
        airports[r['id']] = {
            'n': r['name'], 'c': r['icao_code'] or r['gps_code'] or r['ident'],
            'lat': round(lat, 5), 'lon': round(lon, 5),
            'e': int(float(r['elevation_ft'])) if r['elevation_ft'] else 0,
            't': th, 'big': r['type'] != 'small_airport', 'r': [],
        }

with open(runways_csv, encoding='utf-8') as f:
    for r in csv.DictReader(f):
        a = airports.get(r['airport_ref'])
        if not a or r['closed'] == '1':
            continue
        try:
            la1, lo1 = float(r['le_latitude_deg']), float(r['le_longitude_deg'])
            la2, lo2 = float(r['he_latitude_deg']), float(r['he_longitude_deg'])
            length = int(r['length_ft'] or 0)
        except ValueError:
            continue
        hard = r['surface'].upper().startswith(HARD)
        if length < 2500 or (not hard and not a['big']):
            continue
        a['r'].append([r['le_ident'], r['he_ident'], round(la1, 5), round(lo1, 5), round(la2, 5), round(lo2, 5), length, int(r['width_ft'] or 0)])

keep = [a for a in airports.values() if a['r']]
keep.sort(key=lambda a: (a['t'], a['n']))
rows = [[a['t'], a['n'], a['c'], a['lat'], a['lon'], a['e'], a['r']] for a in keep]
with open(out, 'w', encoding='utf-8', newline='\n') as f:
    json.dump(rows, f, ensure_ascii=False, separators=(',', ':'))
from collections import Counter
print(len(rows), 'airfields', dict(Counter(a['t'] for a in keep)))
