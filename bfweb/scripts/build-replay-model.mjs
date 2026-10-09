// One Sketchfab-style glTF (a single aircraft, vehicle or ship) -> one small
// GLB for the replay theatre: every mesh baked into one frame, nose on +Y, up
// on +Z, centred, scaled to the real length, simplified, geometry only.
//
//   npm i @gltf-transform/core@4 @gltf-transform/functions@4 @gltf-transform/extensions@4 meshoptimizer
//   node build-replay-model.mjs <scene.gltf> <out.glb> --length 41.5 [--nose -z] [--up +y] [--tris 6000]
//
// Orientation: glTF is Y-up, so up defaults to +y; the nose defaults to
// whichever end of the longest horizontal axis looks like the front (the end
// without the tail fin for aircraft; pass --nose when the preview sheet
// (scripts' preview.py) shows it backwards). Credits for every model go in
// public/models/CREDITS.txt.
import { NodeIO } from '@gltf-transform/core'
import { ALL_EXTENSIONS } from '@gltf-transform/extensions'
import { weld, simplify, prune, dedup, transformMesh, join } from '@gltf-transform/functions'
import { MeshoptSimplifier } from 'meshoptimizer'
import fs from 'node:fs'

const args = process.argv.slice(2)
const [SRC, OUT] = args
const opt = k => { const i = args.indexOf(`--${k}`); return i >= 0 ? args[i + 1] : undefined }
const LENGTH = Number(opt('length') ?? 0)
const TRIS = Number(opt('tris') ?? 6000)
if (!SRC || !OUT || !LENGTH) throw new Error('usage: <scene.gltf> <out.glb> --length <m> [--nose ±x|±y|±z] [--up ±x|±y|±z] [--tris n]')
const axis = s => { const m = /^([+-])?([xyz])$/.exec(s); if (!m) throw new Error(`bad axis ${s}`); return { k: 'xyz'.indexOf(m[2]), sign: m[1] === '-' ? -1 : 1 } }

await MeshoptSimplifier.ready
const io = new NodeIO().registerExtensions(ALL_EXTENSIONS)
const doc = await io.read(SRC)
const root = doc.getRoot()
const scene = root.getDefaultScene() ?? root.listScenes()[0]

const mul = (a, b) => { // column-major 4x4
  const o = new Array(16).fill(0)
  for (let c = 0; c < 4; c++) for (let r = 0; r < 4; r++) for (let k = 0; k < 4; k++) o[c * 4 + r] += a[k * 4 + r] * b[c * 4 + k]
  return o
}
const apply = (m, p) => [0, 1, 2].map(r => m[r] * p[0] + m[4 + r] * p[1] + m[8 + r] * p[2] + m[12 + r])

// Bake world transforms; gather the points.
const pts = []
const baked = []
for (const node of root.listNodes()) {
  const mesh = node.getMesh()
  if (!mesh) continue
  const wm = node.getWorldMatrix()
  baked.push({ mesh, wm })
  for (const prim of mesh.listPrimitives()) {
    const pos = prim.getAttribute('POSITION')
    const el = [0, 0, 0]
    for (let i = 0; i < pos.getCount(); i += 3) pts.push(apply(wm, pos.getElement(i, el)))
  }
}
const min = [0, 1, 2].map(k => Math.min(...pts.map(p => p[k])))
const max = [0, 1, 2].map(k => Math.max(...pts.map(p => p[k])))
const centre = [0, 1, 2].map(k => (min[k] + max[k]) / 2)
const ext = [0, 1, 2].map(k => max[k] - min[k])

const up = opt('up') ? axis(opt('up')) : { k: 1, sign: 1 }
let nose
if (opt('nose')) nose = axis(opt('nose'))
else {
  const horiz = [0, 1, 2].filter(k => k !== up.k)
  const k = ext[horiz[0]] >= ext[horiz[1]] ? horiz[0] : horiz[1]
  // The tail is the end that reaches highest (a fin); the nose is the other.
  const top = (sgn) => Math.max(...pts.filter(p => sgn * (p[k] - centre[k]) > ext[k] * 0.3).map(p => up.sign * p[up.k]), -Infinity)
  nose = { k, sign: top(1) > top(-1) ? -1 : 1 }
}
const vec = a => { const v = [0, 0, 0]; v[a.k] = a.sign; return v }
const n = vec(nose), u = vec(up)
const right = [n[1] * u[2] - n[2] * u[1], n[2] * u[0] - n[0] * u[2], n[0] * u[1] - n[1] * u[0]]
const s = LENGTH / ext[nose.k]
const R = [right, n, u]
const M = new Array(16).fill(0)
for (let r = 0; r < 3; r++) {
  for (let c = 0; c < 3; c++) M[c * 4 + r] = R[r][c] * s
  M[12 + r] = -s * (R[r][0] * centre[0] + R[r][1] * centre[1] + R[r][2] * centre[2])
}
M[15] = 1

const out = doc.createNode('model')
for (const { mesh, wm } of baked) {
  const copy = mesh.clone()
  transformMesh(copy, mul(M, wm))
  out.addChild(doc.createNode().setMesh(copy))
}
for (const c of scene.listChildren()) scene.removeChild(c)
scene.addChild(out)
for (const nd of root.listNodes()) if (nd !== out && nd.getParentNode() !== out) nd.dispose()
for (const mesh of root.listMeshes()) for (const p of mesh.listPrimitives()) {
  // geometry only (the replay paints by coalition); normals too, so welding works
  for (const sem of p.listSemantics()) if (sem !== 'POSITION') p.setAttribute(sem, null)
  p.setMaterial(null)
}
await doc.transform(prune(), dedup(), join(), weld())
let tris = 0
for (const mesh of root.listMeshes()) for (const p of mesh.listPrimitives()) tris += (p.getIndices()?.getCount() ?? 0) / 3
await doc.transform(simplify({ simplifier: MeshoptSimplifier, ratio: Math.min(1, TRIS / Math.max(tris, 1)), error: 0.02 }), prune())
await io.write(OUT, doc)
let after = 0
for (const mesh of root.listMeshes()) for (const p of mesh.listPrimitives()) after += (p.getIndices()?.getCount() ?? 0) / 3
console.log(`${OUT}: ${Math.round(tris)} -> ${Math.round(after)} tris, ${fs.statSync(OUT).size} bytes, length ${LENGTH} m, nose ${nose.sign > 0 ? '+' : '-'}${'xyz'[nose.k]}, up ${up.sign > 0 ? '+' : '-'}${'xyz'[up.k]}`)
