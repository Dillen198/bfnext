// Convert Tacview's 3D meshes (github.com/Vyrtuoz/Tacview, "3D Models/Data/
// Meshes", GPL-3.0) into the replay's model files: one small GLB each, nose
// on +Y, up on +Z, origin at the middle, real size in metres, positions only
// (the theatre colours them by coalition and computes its own normals).
//
// Tacview's meshes are already in metres with Y up and the nose on -Z, so
// the conversion is a rotation (x, y, z) -> (x, -z, y) -- right-handed both
// sides, nothing mirrored -- plus recentring. Faces in LOD1+ groups are left
// out; everything else (airframe, canopy, control surfaces) is kept.
//
// Every mesh keeps Tacview's file name (FixedWing.F-16.obj -> FixedWing.F-16.glb):
// which DCS type gets which mesh is Tacview's own database, made into
// shared/models/tacview.json by gen-tacview-db.py and read by
// shared/models/catalog.ts. Credits and the licence are in
// public/models/CREDITS.txt and public/models/LICENSE-GPL-3.0.txt.
//
// usage:
//   node build-tacview-models.mjs <Tacview>/"3D Models/Data/Meshes" <bfweb/public/models>
import fs from 'node:fs'
import path from 'node:path'

const [SRC, OUT] = process.argv.slice(2)
if (!SRC || !OUT) {
  console.error('usage: node build-tacview-models.mjs <meshes dir> <out dir>')
  process.exit(2)
}

function readObj(file) {
  const text = fs.readFileSync(file, 'utf8')
  const verts = []
  const tris = []
  let skip = false
  for (const raw of text.split(/\r?\n/)) {
    const line = raw.trim()
    if (line.startsWith('v ')) {
      const [, x, y, z] = line.split(/\s+/).map(Number)
      verts.push([x, -z, y]) // nose -Z -> +Y, up Y -> +Z
    } else if (line.startsWith('g ') || line.startsWith('o ')) {
      skip = /^LOD[1-9]/i.test(line.slice(2).trim())
    } else if (line.startsWith('f ') && !skip) {
      const idx = line.slice(2).trim().split(/\s+/).map(t => {
        const i = parseInt(t.split('/')[0], 10)
        return i < 0 ? verts.length + i : i - 1
      })
      for (let k = 1; k + 1 < idx.length; k++) tris.push(idx[0], idx[k], idx[k + 1])
    }
  }
  return { verts, tris }
}

function toGlb({ verts, tris }) {
  // recentre on the bounding box
  const min = [Infinity, Infinity, Infinity], max = [-Infinity, -Infinity, -Infinity]
  for (const v of verts) for (let k = 0; k < 3; k++) { min[k] = Math.min(min[k], v[k]); max[k] = Math.max(max[k], v[k]) }
  const c = [0, 1, 2].map(k => (min[k] + max[k]) / 2)
  const pos = new Float32Array(verts.length * 3)
  verts.forEach((v, i) => { for (let k = 0; k < 3; k++) pos[i * 3 + k] = v[k] - c[k] })
  const big = verts.length > 65535
  const ind = big ? new Uint32Array(tris) : new Uint16Array(tris)
  const pad4 = n => (n + 3) & ~3
  const posBytes = pos.byteLength, indOff = pad4(posBytes), binLen = pad4(indOff + ind.byteLength)
  const bin = Buffer.alloc(binLen)
  Buffer.from(pos.buffer).copy(bin, 0)
  Buffer.from(ind.buffer).copy(bin, indOff)
  const gltf = {
    asset: { version: '2.0', generator: 'bfweb build-tacview-models.mjs (meshes: Tacview, GPL-3.0)' },
    scene: 0, scenes: [{ nodes: [0] }], nodes: [{ mesh: 0 }],
    meshes: [{ primitives: [{ attributes: { POSITION: 0 }, indices: 1 }] }],
    buffers: [{ byteLength: binLen }],
    bufferViews: [
      { buffer: 0, byteOffset: 0, byteLength: posBytes, target: 34962 },
      { buffer: 0, byteOffset: indOff, byteLength: ind.byteLength, target: 34963 },
    ],
    accessors: [
      { bufferView: 0, componentType: 5126, count: verts.length, type: 'VEC3', min: min.map((m, k) => m - c[k]), max: max.map((m, k) => m - c[k]) },
      { bufferView: 1, componentType: big ? 5125 : 5123, count: ind.length, type: 'SCALAR' },
    ],
  }
  let json = Buffer.from(JSON.stringify(gltf))
  json = Buffer.concat([json, Buffer.alloc(pad4(json.length) - json.length, 0x20)])
  const total = 12 + 8 + json.length + 8 + bin.length
  const head = Buffer.alloc(12)
  head.writeUInt32LE(0x46546c67, 0); head.writeUInt32LE(2, 4); head.writeUInt32LE(total, 8)
  const ch = (len, type) => { const b = Buffer.alloc(8); b.writeUInt32LE(len, 0); b.writeUInt32LE(type, 4); return b }
  return { glb: Buffer.concat([head, ch(json.length, 0x4e4f534a), json, ch(bin.length, 0x004e4942), bin]), size: max.map((m, k) => m - min[k]) }
}

fs.mkdirSync(OUT, { recursive: true })
let total = 0
for (const f of fs.readdirSync(SRC).filter(f => f.toLowerCase().endsWith('.obj')).sort()) {
  const name = f.slice(0, -4)
  const obj = readObj(path.join(SRC, f))
  if (!obj.tris.length) { console.warn(`${name}: no faces, skipped`); continue }
  const { glb, size } = toGlb(obj)
  fs.writeFileSync(path.join(OUT, `${name}.glb`), glb)
  total += glb.length
  console.log(`${name.padEnd(44)} ${String(obj.tris.length / 3).padStart(6)} tris  ${size.map(x => x.toFixed(1)).join(' x ')} m  ${(glb.length / 1024).toFixed(0)} KB`)
}
console.log(`total ${(total / 1048576).toFixed(1)} MB`)
