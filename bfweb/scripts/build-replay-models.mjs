// Split the "FREE - Fighter Jet Collection - Low Poly" pack (bohmerang,
// CC-BY-4.0) into one small GLB per aircraft for the replay theatre:
// airframe + canopy + gear-up parts only, transforms baked, nose on +Y, up on
// +Z, origin at the middle, real length in metres, simplified.
// usage (from any scratch folder with the tools installed):
//   npm i @gltf-transform/core@4 @gltf-transform/functions@4 @gltf-transform/extensions@4 meshoptimizer
//   node build-replay-models.mjs <scene.gltf> <bfweb/public/models>
// Credits for anything added go in public/models/CREDITS.txt.
import { NodeIO } from '@gltf-transform/core'
import { ALL_EXTENSIONS } from '@gltf-transform/extensions'
import { weld, simplify, prune, dedup, transformMesh, join } from '@gltf-transform/functions'
import { MeshoptSimplifier } from 'meshoptimizer'
import fs from 'node:fs'
import path from 'node:path'

const [SRC, OUT] = process.argv.slice(2)
fs.mkdirSync(OUT, { recursive: true })
await MeshoptSimplifier.ready
const io = new NodeIO().registerExtensions(ALL_EXTENSIONS)

// real overall length, metres
const PLANES = { 'F-14': 19.1, 'F-15': 19.43, 'F-16': 15.06, 'F-18': 17.1, 'F-22': 18.92, 'F-35': 15.67 }
const KEEP = new Set(['Airframe', 'Canopy', 'LandingOff', 'WeaponRails'])
const TARGET_TRIS = 6000
const PART = /^(.*?)(Airframe|Canopy|Cockpit|InstrGlass|Hud|LandingOff|LandingOn|WeaponRails)_\d+$/

function partOf(node) {
  for (let n = node; n; n = n.getParentNode()) {
    const m = n.getName().match(PART)
    if (m) return { plane: m[1], part: m[2] }
  }
  return null
}

function apply(m, p) { // column-major 4x4 * point
  return [
    m[0] * p[0] + m[4] * p[1] + m[8] * p[2] + m[12],
    m[1] * p[0] + m[5] * p[1] + m[9] * p[2] + m[13],
    m[2] * p[0] + m[6] * p[1] + m[10] * p[2] + m[14],
  ]
}

function stats(doc, filter) {
  const min = [Infinity, Infinity, Infinity], max = [-Infinity, -Infinity, -Infinity]
  const sum = [0, 0, 0]
  let n = 0
  for (const node of doc.getRoot().listNodes()) {
    const mesh = node.getMesh()
    if (!mesh || !filter(node)) continue
    const wm = node.getWorldMatrix()
    for (const prim of mesh.listPrimitives()) {
      const pos = prim.getAttribute('POSITION')
      const el = [0, 0, 0]
      for (let i = 0; i < pos.getCount(); i++) {
        const p = apply(wm, pos.getElement(i, el))
        for (let k = 0; k < 3; k++) { min[k] = Math.min(min[k], p[k]); max[k] = Math.max(max[k], p[k]); sum[k] += p[k] }
        n++
      }
    }
  }
  return { min, max, centroid: sum.map(s => s / Math.max(n, 1)), n }
}

function tris(doc) {
  let t = 0
  for (const mesh of doc.getRoot().listMeshes()) for (const p of mesh.listPrimitives()) t += (p.getIndices()?.getCount() ?? p.getAttribute('POSITION').getCount()) / 3
  return Math.round(t)
}

const credits = []
for (const [plane, length] of Object.entries(PLANES)) {
  const doc = await io.read(SRC)
  const root = doc.getRoot()
  const scene = root.getDefaultScene() ?? root.listScenes()[0]
  const mine = node => { const p = partOf(node); return p && p.plane === plane }
  const kept = node => { const p = partOf(node); return p && p.plane === plane && KEEP.has(p.part) }

  // Axes from the geometry itself: longest = fuselage, shortest = up; the
  // cockpit says which end is the nose, the canopy which side is up.
  const body = stats(doc, kept)
  const cockpit = stats(doc, node => { const p = partOf(node); return p && p.plane === plane && p.part === 'Cockpit' })
  const canopy = stats(doc, node => { const p = partOf(node); return p && p.plane === plane && p.part === 'Canopy' })
  const ext = [0, 1, 2].map(k => body.max[k] - body.min[k])
  const centre = [0, 1, 2].map(k => (body.max[k] + body.min[k]) / 2)
  const order = [0, 1, 2].sort((a, b) => ext[b] - ext[a])
  // The fuselage is one of the two long axes -- the one the cockpit sits far
  // out along (an F-14 with its wings forward is wider than it is long).
  const off = k => Math.abs(cockpit.centroid[k] - centre[k])
  const noseAx = off(order[0]) >= off(order[1]) ? order[0] : order[1]
  const upAx = order[2]
  const noseSign = Math.sign(cockpit.centroid[noseAx] - centre[noseAx]) || 1
  // against the mass centre, not the box centre: a tall fin lifts the box
  const upSign = Math.sign(canopy.centroid[upAx] - body.centroid[upAx]) || 1
  const nose = [0, 0, 0]; nose[noseAx] = noseSign
  const up = [0, 0, 0]; up[upAx] = upSign
  const right = [nose[1] * up[2] - nose[2] * up[1], nose[2] * up[0] - nose[0] * up[2], nose[0] * up[1] - nose[1] * up[0]]
  const s = length / ext[noseAx]
  // rows of R: right, nose, up -> new x, y, z; M = S * R * T(-centre), column-major
  const R = [right, nose, up]
  const M = new Array(16).fill(0)
  for (let r = 0; r < 3; r++) {
    for (let c = 0; c < 3; c++) M[c * 4 + r] = R[r][c] * s
    M[12 + r] = -s * (R[r][0] * centre[0] + R[r][1] * centre[1] + R[r][2] * centre[2])
  }
  M[15] = 1

  // Bake every kept mesh into one node at the scene root; drop the rest.
  const out = doc.createNode(plane)
  for (const node of root.listNodes()) {
    const mesh = node.getMesh()
    if (!mesh || !kept(node)) continue
    const wm = node.getWorldMatrix()
    // world, then our frame
    const full = new Array(16).fill(0)
    for (let c = 0; c < 4; c++) for (let r = 0; r < 4; r++) {
      let v = 0
      for (let k = 0; k < 4; k++) v += M[k * 4 + r] * wm[c * 4 + k]
      full[c * 4 + r] = v
    }
    const copy = mesh.clone()
    transformMesh(copy, full)
    out.addChild(doc.createNode().setMesh(copy))
  }
  for (const c of scene.listChildren()) scene.removeChild(c)
  scene.addChild(out)
  for (const n of root.listNodes()) if (!mine(n) && n !== out && n.getParentNode() !== out) n.dispose()
  // geometry only: the replay paints models in coalition colours
  for (const mesh of root.listMeshes()) for (const p of mesh.listPrimitives()) {
    for (const sem of p.listSemantics()) if (sem !== 'POSITION') p.setAttribute(sem, null) // normals too: split normals stop welding; the viewer recomputes them
  }
  await doc.transform(prune(), dedup(), join(), weld())
  const before = tris(doc)
  await doc.transform(simplify({ simplifier: MeshoptSimplifier, ratio: Math.min(1, TARGET_TRIS / before), error: 0.02 }), prune())
  const file = path.join(OUT, `${plane}.glb`)
  await io.write(file, doc)
  const after = stats(doc, () => true)
  console.log(`${plane}: ${before} -> ${tris(doc)} tris, ${fs.statSync(file).size} bytes, ` +
    `x ${after.min[0].toFixed(1)}..${after.max[0].toFixed(1)} y ${after.min[1].toFixed(1)}..${after.max[1].toFixed(1)} z ${after.min[2].toFixed(1)}..${after.max[2].toFixed(1)} ` +
    `(nose axis ${noseAx}${noseSign > 0 ? '+' : '-'}, up ${upAx}${upSign > 0 ? '+' : '-'})`)
  credits.push(plane)
}
console.log('built', credits.join(', '))
