// One Sketchfab-style glTF (a single aircraft, vehicle or ship) -> one small
// GLB for the replay theatre: every mesh baked into one frame, nose on +Y, up
// on +Z, centred, scaled to the real length, simplified, geometry only.
//
//   npm i @gltf-transform/core@4 @gltf-transform/functions@4 @gltf-transform/extensions@4 meshoptimizer
//   node build-replay-model.mjs <scene.gltf> <out.glb> --length 41.5 [--kind air|helo|ground|ship] [--flip] [--turn 90|-90|180] [--nose -z] [--up +y] [--tris 6000]
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
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
const unit = a => { const v = [0, 0, 0]; v[a.k] = a.sign; return v }
const KIND = opt('kind') ?? 'air' // air | helo | ground | ship

// Up: glTF is Y-up.
const U = unit(opt('up') ? axis(opt('up')) : { k: 1, sign: 1 })
const others = [0, 1, 2].filter(k => U[k] === 0)
const h1 = unit({ k: others[0], sign: 1 }), h2 = unit({ k: others[1], sign: 1 })
// a sample is plenty for the shape analysis
const sample = pts.length > 40_000 ? pts.filter((_, i) => i % Math.ceil(pts.length / 40_000) === 0) : pts

function range(dir, set = sample) {
  let lo = Infinity, hi = -Infinity
  for (const p of set) { const d = dot(p, dir); if (d < lo) lo = d; if (d > hi) hi = d }
  return [lo, hi]
}

let F // fuselage direction (unsigned yet)
if (opt('nose')) F = unit(axis(opt('nose')))
else {
  // Models are not always built square to their axes. The fuselage is the
  // line the top view is mirror-symmetric about (aircraft, vehicles and ships
  // all are, left to right -- never front to back), so try every heading and
  // keep the one whose silhouette best matches its own mirror image.
  // (a helicopter's rotor disc, at whatever angle the blades were left, is
  // not part of the shape: use what is below it)
  const [uu0, uu1] = range(U)
  const body = KIND === 'helo' ? sample.filter(p => dot(p, U) < uu0 + (uu1 - uu0) * 0.75) : sample
  const xy = body.map(p => [dot(p, h1), dot(p, h2)])
  const cx = xy.reduce((a, q) => a + q[0], 0) / xy.length
  const cy = xy.reduce((a, q) => a + q[1], 0) / xy.length
  let rmax = 0
  for (const q of xy) rmax = Math.max(rmax, Math.hypot(q[0] - cx, q[1] - cy))
  const N = 128
  const score = th => {
    const c = Math.cos(th), sn = Math.sin(th)
    // across-coordinate of the mirror line: the centroid, on the axis for a symmetric shape
    let off = 0
    for (const q of xy) off += -(q[0] - cx) * sn + (q[1] - cy) * c
    off /= xy.length
    const g = new Uint8Array(N * N)
    for (const q of xy) {
      const along = (q[0] - cx) * c + (q[1] - cy) * sn
      const across = -(q[0] - cx) * sn + (q[1] - cy) * c - off
      const i = Math.min(N - 1, Math.max(0, Math.floor(((across / rmax) + 1) / 2 * N)))
      const j = Math.min(N - 1, Math.max(0, Math.floor(((along / rmax) + 1) / 2 * N)))
      g[j * N + i] = 1
    }
    let both = 0, all = 0
    for (let j = 0; j < N; j++) for (let i = 0; i < N; i++) if (g[j * N + i]) { all++; if (g[j * N + N - 1 - i]) both++ }
    return both / (all || 1)
  }
  let best = { sc: -1, th: 0 }
  for (let deg = 0; deg < 180; deg += 1) { const th = deg * Math.PI / 180; const sc = score(th); if (sc > best.sc) best = { sc, th } }
  for (let d = -1; d <= 1; d += 0.1) { const th = best.th + d * Math.PI / 180; const sc = score(th); if (sc > best.sc) best = { sc, th } }
  F = [0, 1, 2].map(k => Math.cos(best.th) * h1[k] + Math.sin(best.th) * h2[k])
}

// Which end of F is the front.
const [f0, f1] = range(F)
const flen = f1 - f0
const R0 = cross(F, U)
const ends = sgn => sample.filter(p => sgn * (dot(p, F) - (f0 + f1) / 2) > flen * (KIND === 'air' ? 0.3 : 0.4))
let noseSign
if (KIND === 'helo') {
  // The cabin end carries the bulk; the tail boom is thin. Leave out the
  // rotor disc, which spans both ends.
  const [u0, u1] = range(U)
  const low = set => set.filter(p => dot(p, U) < u0 + (u1 - u0) * 0.7).length
  noseSign = low(ends(1)) >= low(ends(-1)) ? 1 : -1
} else if (KIND === 'ship') {
  // The bow is the narrow end.
  const width = set => { const [a, b] = range(R0, set.length ? set : sample); return b - a }
  noseSign = width(ends(1)) <= width(ends(-1)) ? 1 : -1
} else if (KIND === 'ground') {
  // A gun barrel: the end with the fewest points (one thin tube).
  noseSign = ends(1).length <= ends(-1).length ? 1 : -1
} else {
  // Aircraft: the tail is the end that reaches highest (a fin).
  const top = set => { let t = -Infinity; for (const p of set) t = Math.max(t, dot(p, U)); return t }
  noseSign = top(ends(1)) > top(ends(-1)) ? -1 : 1
}
if (args.includes('--flip')) noseSign = -noseSign
let n = F.map(x => x * noseSign)
// --turn 90|-90|180: when the preview shows the nose off to one side
// (a tank hull is nearly as symmetric fore-aft as left-right)
const turn = Number(opt('turn') ?? 0)
if (turn === 90) n = cross(n, U)
else if (turn === -90) n = cross(U, n)
else if (turn === 180) n = n.map(x => -x)
const right = cross(n, U)
// length along the final nose direction
const [n0, n1] = range(n, pts)
const s = LENGTH / (n1 - n0)
const R = [right, n, U]
const centre = [0, 0, 0]
for (const dir of R) { const [lo, hi] = range(dir, pts); for (let k = 0; k < 3; k++) centre[k] += dir[k] * (lo + hi) / 2 }
const M = new Array(16).fill(0)
for (let r = 0; r < 3; r++) {
  for (let c = 0; c < 3; c++) M[c * 4 + r] = R[r][c] * s
  M[12 + r] = -s * (R[r][0] * centre[0] + R[r][1] * centre[1] + R[r][2] * centre[2])
}
M[15] = 1
const nose = { label: n.map(v => v.toFixed(2)).join(',') }

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
// rotor/turret animations, skins, textures and materials would otherwise
// survive prune() through the animation channels and bloat the file
for (const a of root.listAnimations()) a.dispose()
for (const k of root.listSkins()) k.dispose()
for (const m of root.listMaterials()) m.dispose()
for (const t of root.listTextures()) t.dispose()
for (const c of root.listCameras()) c.dispose()
await doc.transform(prune(), dedup(), join(), weld())
let tris = 0
for (const mesh of root.listMeshes()) for (const p of mesh.listPrimitives()) tris += (p.getIndices()?.getCount() ?? 0) / 3
await doc.transform(simplify({ simplifier: MeshoptSimplifier, ratio: Math.min(1, TRIS / Math.max(tris, 1)), error: 0.02 }), prune())
await io.write(OUT, doc)
let after = 0
for (const mesh of root.listMeshes()) for (const p of mesh.listPrimitives()) after += (p.getIndices()?.getCount() ?? 0) / 3
console.log(`${OUT}: ${Math.round(tris)} -> ${Math.round(after)} tris, ${fs.statSync(OUT).size} bytes, length ${LENGTH} m, nose (${nose.label}), kind ${KIND}`)
