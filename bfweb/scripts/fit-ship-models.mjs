// Where the flight deck, the keel and the ends are in each ship model, so a
// site can sit the model on the water (and a carrier's deck on the deck its
// grading uses). The models are origin-centred (see build-replay-model.mjs),
// so none of that is in the file itself.
//
//   node scripts/fit-ship-models.mjs > ../shared/models/ship-fit.json
//
// deck_z: the height with the most upward-facing area (the flight deck on a
// carrier, the main deck on anything else); keel_x: the middle of the hull
// bottom; stern_y / bow_y: the ends along +Y; bottom_z: the keel.
import fs from 'node:fs'
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js'
import * as THREE from 'three'

const DIR = new URL('../public/models/', import.meta.url)
// every ship model: Tacview's watercraft meshes and its generic hulls
const SHIPS = fs.readdirSync(DIR).filter(f => /^(Watercraft\.|Core\.(Watercraft|AircraftCarrier)).*\.glb$/.test(f)).map(f => f.slice(0, -4)).sort()

async function load(name) {
  const buf = fs.readFileSync(new URL(`${encodeURIComponent(name)}.glb`, DIR))
  const ab = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength)
  return new Promise((res, rej) => new GLTFLoader().parse(ab, '', g => res(g.scene), rej))
}

const out = {}
for (const name of SHIPS) {
  const scene = await load(name)
  scene.updateMatrixWorld(true)
  const tris = []
  scene.traverse(o => {
    if (!o.isMesh) return
    const g = o.geometry.index ? o.geometry.toNonIndexed() : o.geometry
    const p = g.attributes.position
    const v = [new THREE.Vector3(), new THREE.Vector3(), new THREE.Vector3()]
    for (let i = 0; i < p.count; i += 3) {
      for (let k = 0; k < 3; k++) v[k].fromBufferAttribute(p, i + k).applyMatrix4(o.matrixWorld)
      tris.push(v.map(x => x.clone()))
    }
  })
  const box = new THREE.Box3()
  for (const t of tris) for (const v of t) box.expandByPoint(v)
  // area of upward faces, by height (25 cm bins), above the middle of the hull
  const bins = new Map()
  const e1 = new THREE.Vector3(), e2 = new THREE.Vector3(), nrm = new THREE.Vector3()
  for (const [a, b, c] of tris) {
    nrm.crossVectors(e1.subVectors(b, a), e2.subVectors(c, a))
    const area = nrm.length() / 2
    if (area <= 0) continue
    nrm.normalize()
    const z = (a.z + b.z + c.z) / 3
    if (nrm.z < 0.97 || z < (box.min.z + box.max.z) / 2 - 15) continue
    const k = Math.round(z * 4) / 4
    bins.set(k, (bins.get(k) ?? 0) + area)
  }
  let deck = 0, best = 0
  for (const [k, a] of bins) if (a > best) { best = a; deck = k }
  // the keel: the middle of everything within 1.5 m of the bottom
  let sx = 0, n = 0
  for (const t of tris) for (const v of t) if (v.z < box.min.z + 1.5) { sx += v.x; n++ }
  out[name] = {
    stern_y: +box.min.y.toFixed(2),
    bow_y: +box.max.y.toFixed(2),
    keel_x: +(n ? sx / n : 0).toFixed(2),
    deck_z: deck,
    bottom_z: +box.min.z.toFixed(2),
    top_z: +box.max.z.toFixed(2),
    deck_area_m2: Math.round(best),
  }
}
process.stdout.write(JSON.stringify(out, null, 1) + '\n')
