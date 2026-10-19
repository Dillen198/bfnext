// The page backdrops: line drawings traced from the site's real 3D models
// (bfweb/public/models, credits in CREDITS.txt there), each from one fixed
// view. The model's feature edges (where faces meet at more than a set
// angle) are projected orthographically and written out as one SVG path,
// with the hidden half of every edge left out by a coarse depth test, so it
// reads as a drawing rather than an x-ray.
//
//   node scripts/wireframes.mjs
//
// writes src/assets/backdrops/<name>.svg. Run it again after changing a view.
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import * as THREE from 'three'
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js'
import { mergeGeometries } from 'three/examples/jsm/utils/BufferGeometryUtils.js'

const MODELS = fileURLToPath(new URL('../../bfweb/public/models/', import.meta.url))
const OUT = fileURLToPath(new URL('../src/assets/backdrops/', import.meta.url))

/** name, model, view: azimuth (deg, 0 = from dead ahead, + = from the right), elevation (deg), crease angle, width px, line width px */
const VIEWS = [
  { name: 'hornet', model: 'F-18', az: -38, el: 22, edge: 18, w: 1400, stroke: 2.2 },
  { name: 'tomcat', model: 'F-14', az: 0, el: 90, edge: 18, w: 1200, stroke: 2 },
  { name: 'carrier', model: 'CVN', az: -148, el: 24, edge: 28, w: 1800, stroke: 1.6 },
]

async function load(model) {
  const buf = readFileSync(`${MODELS}${model}.glb`)
  const ab = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength)
  const gltf = await new Promise((res, rej) => new GLTFLoader().parse(ab, '', res, rej))
  const parts = []
  gltf.scene.updateMatrixWorld(true)
  gltf.scene.traverse(o => {
    if (!o.isMesh) return
    const g = o.geometry.clone().applyMatrix4(o.matrixWorld)
    for (const k of Object.keys(g.attributes)) if (k !== 'position') g.deleteAttribute(k)
    parts.push(g.index ? g.toNonIndexed() : g)
  })
  return mergeGeometries(parts)
}

/** Crease, open and silhouette edges of a triangle soup, as [x y z x y z]... */
function edgeLines(geo, creaseDeg, toCam) {
  const pos = geo.attributes.position
  geo.computeBoundingSphere()
  const q = geo.boundingSphere.radius * 1e-4
  const ids = new Map()
  const vid = i => {
    const k = `${Math.round(pos.getX(i) / q)},${Math.round(pos.getY(i) / q)},${Math.round(pos.getZ(i) / q)}`
    let id = ids.get(k)
    if (id === undefined) { id = ids.size; ids.set(k, id) }
    return id
  }
  const nf = pos.count / 3
  const normals = []
  const faces = new Map()
  const A = new THREE.Vector3(), B = new THREE.Vector3(), C = new THREE.Vector3()
  for (let t = 0; t < nf; t++) {
    A.fromBufferAttribute(pos, t * 3); B.fromBufferAttribute(pos, t * 3 + 1); C.fromBufferAttribute(pos, t * 3 + 2)
    const n = new THREE.Vector3().subVectors(B, A).cross(new THREE.Vector3().subVectors(C, A))
    if (n.lengthSq() < 1e-20) { normals.push(null); continue }
    normals.push(n.normalize())
    const v = [vid(t * 3), vid(t * 3 + 1), vid(t * 3 + 2)]
    for (let j = 0; j < 3; j++) {
      const a = v[j], b = v[(j + 1) % 3]
      if (a === b) continue
      const k = a < b ? `${a}_${b}` : `${b}_${a}`
      let e = faces.get(k)
      if (!e) { e = { f: [], i: t * 3 + j, i2: t * 3 + ((j + 1) % 3) }; faces.set(k, e) }
      e.f.push(t)
    }
  }
  const cos = Math.cos(THREE.MathUtils.degToRad(creaseDeg))
  const out = []
  for (const e of faces.values()) {
    let keep = e.f.length !== 2
    if (!keep) {
      const [n1, n2] = [normals[e.f[0]], normals[e.f[1]]]
      keep = n1.dot(n2) < cos || Math.sign(n1.dot(toCam)) !== Math.sign(n2.dot(toCam))
    }
    if (keep) out.push(pos.getX(e.i), pos.getY(e.i), pos.getZ(e.i), pos.getX(e.i2), pos.getY(e.i2), pos.getZ(e.i2))
  }
  return out
}

function draw(geo, v) {
  // the camera: models face +Y, Z up, X to starboard
  const az = THREE.MathUtils.degToRad(v.az), el = THREE.MathUtils.degToRad(v.el)
  const dir = new THREE.Vector3(Math.sin(az) * Math.cos(el), Math.cos(az) * Math.cos(el), Math.sin(el))
  const cam = new THREE.OrthographicCamera(-1, 1, 1, -1, 0.1, 10000)
  cam.up.set(0, 0, 1)
  if (v.el >= 89) cam.up.set(0, 1, 0)
  geo.computeBoundingSphere()
  const c = geo.boundingSphere.center, r = geo.boundingSphere.radius
  cam.position.copy(c).addScaledVector(dir, r * 4)
  cam.lookAt(c)
  cam.updateMatrixWorld(true)
  const view = cam.matrixWorldInverse

  // project every vertex: x right, y down, depth (bigger = further)
  const pos = geo.attributes.position
  const P = new Float32Array(pos.count * 3)
  const t = new THREE.Vector3()
  let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity
  for (let i = 0; i < pos.count; i++) {
    t.fromBufferAttribute(pos, i).applyMatrix4(view)
    P[i * 3] = t.x; P[i * 3 + 1] = -t.y; P[i * 3 + 2] = -t.z
    x0 = Math.min(x0, t.x); x1 = Math.max(x1, t.x); y0 = Math.min(y0, -t.y); y1 = Math.max(y1, -t.y)
  }
  const pad = 0.02 * (x1 - x0)
  const s = v.w / (x1 - x0 + 2 * pad)
  const W = v.w, H = Math.round((y1 - y0 + 2 * pad) * s)
  const sx = x => (x - x0 + pad) * s, sy = y => (y - y0 + pad) * s

  // a depth buffer of the triangles, at a quarter of the drawing's size
  const Q = 0.25
  const bw = Math.ceil(W * Q) + 1, bh = Math.ceil(H * Q) + 1
  const zb = new Float32Array(bw * bh).fill(Infinity)
  for (let i = 0; i < pos.count; i += 3) {
    const ax = sx(P[i * 3]) * Q, ay = sy(P[i * 3 + 1]) * Q, az_ = P[i * 3 + 2]
    const bx = sx(P[i * 3 + 3]) * Q, by = sy(P[i * 3 + 4]) * Q, bz = P[i * 3 + 5]
    const cx = sx(P[i * 3 + 6]) * Q, cy = sy(P[i * 3 + 7]) * Q, cz = P[i * 3 + 8]
    const minX = Math.max(0, Math.floor(Math.min(ax, bx, cx))), maxX = Math.min(bw - 1, Math.ceil(Math.max(ax, bx, cx)))
    const minY = Math.max(0, Math.floor(Math.min(ay, by, cy))), maxY = Math.min(bh - 1, Math.ceil(Math.max(ay, by, cy)))
    const d = (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)
    if (Math.abs(d) < 1e-9) continue
    for (let py = minY; py <= maxY; py++) {
      for (let px = minX; px <= maxX; px++) {
        const w1 = ((bx - px) * (cy - py) - (cx - px) * (by - py)) / d
        const w2 = ((cx - px) * (ay - py) - (ax - px) * (cy - py)) / d
        const w3 = 1 - w1 - w2
        if (w1 < -0.02 || w2 < -0.02 || w3 < -0.02) continue
        const z = w1 * az_ + w2 * bz + w3 * cz
        const k = py * bw + px
        if (z < zb[k]) zb[k] = z
      }
    }
  }
  const eps = r * 0.012
  const visible = (x, y, z) => {
    const px = Math.round(sx(x) * Q), py = Math.round(sy(y) * Q)
    let m = Infinity
    for (let dy = -1; dy <= 1; dy++) for (let dx = -1; dx <= 1; dx++) {
      const k = (py + dy) * bw + (px + dx)
      if (k >= 0 && k < zb.length) m = Math.min(m, zb[k])
    }
    return z <= m + eps
  }

  // the lines: creases (faces meeting at more than v.edge degrees), open
  // edges, and the silhouette (one face towards the camera, one away), cut
  // into short steps and kept where they are seen
  const lines = edgeLines(geo, v.edge, dir)
  const e = new THREE.Vector3(), f = new THREE.Vector3()
  const segs = []
  for (let i = 0; i < lines.length; i += 6) {
    e.set(lines[i], lines[i + 1], lines[i + 2]).applyMatrix4(view)
    f.set(lines[i + 3], lines[i + 4], lines[i + 5]).applyMatrix4(view)
    const a = [e.x, -e.y, -e.z], b = [f.x, -f.y, -f.z]
    const len = Math.hypot(sx(b[0]) - sx(a[0]), sy(b[1]) - sy(a[1]))
    if (len < 0.6) continue
    const n = Math.max(1, Math.ceil(len / 6))
    let start = null
    for (let j = 0; j <= n; j++) {
      const u = j / n
      const p = [a[0] + (b[0] - a[0]) * u, a[1] + (b[1] - a[1]) * u, a[2] + (b[2] - a[2]) * u]
      const vis = visible(p[0], p[1], p[2])
      if (vis && !start) start = p
      if ((!vis || j === n) && start) {
        const end = vis ? p : [a[0] + (b[0] - a[0]) * ((j - 1) / n), a[1] + (b[1] - a[1]) * ((j - 1) / n)]
        if (Math.hypot(sx(end[0]) - sx(start[0]), sy(end[1]) - sy(start[1])) > 0.8) segs.push([sx(start[0]), sy(start[1]), sx(end[0]), sy(end[1])])
        start = null
      }
    }
  }
  const r1 = n => Math.round(n * 2) / 2
  const d = segs.map(([a, b, c2, d2]) => `M${r1(a)} ${r1(b)}L${r1(c2)} ${r1(d2)}`).join('')
  return { svg: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${W} ${H}" fill="none" stroke="#000" stroke-width="${v.stroke}" stroke-linecap="round"><path d="${d}"/></svg>\n`, segs: segs.length, W, H }
}

mkdirSync(OUT, { recursive: true })
for (const v of VIEWS) {
  const geo = await load(v.model)
  const { svg, segs, W, H } = draw(geo, v)
  writeFileSync(`${OUT}${v.name}.svg`, svg)
  console.log(`${v.name}: ${segs} lines, ${W}x${H}, ${(svg.length / 1024).toFixed(0)} KB`)
}
