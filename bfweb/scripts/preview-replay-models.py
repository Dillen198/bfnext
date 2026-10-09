"""Render top / side / front silhouettes of the replay GLBs to one PNG, to
check orientation (nose up in the top view, canopy up in the side view).
usage: python -I preview.py <models dir> <out.png>"""
import json
import struct
import sys
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw


def load_glb(p):
    b = Path(p).read_bytes()
    _, _, _ = struct.unpack_from('<III', b, 0)
    off = 12
    js = binc = None
    while off < len(b):
        ln, typ = struct.unpack_from('<II', b, off)
        chunk = b[off + 8: off + 8 + ln]
        if typ == 0x4E4F534A:
            js = json.loads(chunk)
        elif typ == 0x004E4942:
            binc = chunk
        off += 8 + ln
    tris = []
    for mesh in js['meshes']:
        for prim in mesh['primitives']:
            acc = js['accessors'][prim['attributes']['POSITION']]
            bv = js['bufferViews'][acc['bufferView']]
            start = bv.get('byteOffset', 0) + acc.get('byteOffset', 0)
            pos = np.frombuffer(binc, dtype='<f4', count=acc['count'] * 3, offset=start).reshape(-1, 3)
            if 'indices' in prim:
                ia = js['accessors'][prim['indices']]
                ibv = js['bufferViews'][ia['bufferView']]
                dt = {5121: '<u1', 5123: '<u2', 5125: '<u4'}[ia['componentType']]
                idx = np.frombuffer(binc, dtype=dt, count=ia['count'], offset=ibv.get('byteOffset', 0) + ia.get('byteOffset', 0))
            else:
                idx = np.arange(len(pos))
            tris.append(pos[idx].reshape(-1, 3, 3))
    return np.concatenate(tris)


def view(t, axes, flip_y=True, size=220, label=''):
    img = Image.new('RGB', (size, size), (24, 28, 20))
    d = ImageDraw.Draw(img)
    pts = t[:, :, axes]
    lim = 11.0
    s = (size - 20) / (2 * lim)
    for tri in pts:
        xy = [(size / 2 + x * s, size / 2 - y * s if flip_y else size / 2 + y * s) for x, y in tri]
        d.polygon(xy, fill=(150, 190, 90))
    d.text((4, 4), label, fill=(255, 255, 255))
    return img


models = sorted(Path(sys.argv[1]).glob('*.glb'))
sheet = Image.new('RGB', (660, 220 * len(models)), (0, 0, 0))
for i, m in enumerate(models):
    t = load_glb(m)
    sheet.paste(view(t, [0, 1], label=f'{m.stem} top (nose up)'), (0, i * 220))
    sheet.paste(view(t, [1, 2], label=f'{m.stem} side (nose right)'), (220, i * 220))
    sheet.paste(view(t, [0, 2], label=f'{m.stem} front'), (440, i * 220))
sheet.save(sys.argv[2])
print('saved', sys.argv[2])
