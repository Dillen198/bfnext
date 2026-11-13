"""Turn Tacview's object database (github.com/Vyrtuoz/Tacview, "Database/
Default Properties", GPL-3.0) into shared/models/tacview.json: for every type
Tacview knows, the names it is matched by, its short and long names, its 3D
mesh and its engagement and detection ranges -- each with the inheritance
along `Base` resolved, as Tacview resolves it.

The output is what shared/models/catalog.ts looks recorded unit types up in:

    {
      "shapes": [mesh name, ...],                       # Tacview mesh file names, without .obj
      "types":  [[short, long, shape index, engagement m or 0, detection m or 0], ...],
      "names":  {normalised name: type index, ...}      # lower case, no spaces, _ - or .
    }

usage: python gen-tacview-db.py "<Tacview>/Database/Default Properties" <out.json> [<Tacview>/3D Models/Data/Meshes]
(with the meshes folder, shape names are spelt as the mesh files are: the
database is not always the same case as the files, which matters on the web)
"""
import glob
import json
import os
import re
import sys
import xml.etree.ElementTree as ET

src, out = sys.argv[1], sys.argv[2]
meshes = {f[:-4].lower(): f[:-4] for f in os.listdir(sys.argv[3]) if f.lower().endswith('.obj')} if len(sys.argv) > 3 else {}

ents = {}
for f in sorted(glob.glob(os.path.join(src, 'Tacview.DefaultProperties.*.xml'))):
    for dp in ET.parse(f).getroot().iter('DefaultProperties'):
        crit, props = dp.find('Criteria'), dp.find('Properties')
        ents[dp.get('Id')] = {
            'base': dp.get('Base'),
            'names': [n.text.strip() for n in crit.findall('Name') if n.text] if crit is not None else [],
            'props': {p.tag: (p.text or '').strip() for p in props} if props is not None else {},
        }


def resolved(i, seen=()):
    e = ents.get(i)
    if not e or i in seen:
        return {}
    r = dict(resolved(e['base'], seen + (i,))) if e['base'] else {}
    r.update({k: v for k, v in e['props'].items() if v})
    return r


def norm(s):
    return re.sub(r'[\s_.\-]+', '', s).lower()


def metres(v):
    try:
        return max(0, int(round(float(v))))
    except (TypeError, ValueError):
        return 0


shapes, shape_ix, types, names = [], {}, [], {}
for i, e in ents.items():
    if not e['names']:
        continue
    p = resolved(i)
    shape = p.get('Shape', '')
    if not shape.lower().endswith('.obj'):
        continue
    shape = meshes.get(shape[:-4].lower(), shape[:-4])
    if shape not in shape_ix:
        shape_ix[shape] = len(shapes)
        shapes.append(shape)
    t = len(types)
    types.append([p.get('ShortName', ''), p.get('LongName', ''), shape_ix[shape],
                  metres(p.get('EngagementRange')), metres(p.get('DetectionRange'))])
    for n in e['names']:
        names.setdefault(norm(n), t)  # first declaration wins, as in Tacview

with open(out, 'w', encoding='utf-8', newline='\n') as f:
    json.dump({'shapes': shapes, 'types': types, 'names': names}, f, ensure_ascii=False, separators=(',', ':'))
print(f'{len(types)} types, {len(names)} names, {len(shapes)} meshes -> {out}')
