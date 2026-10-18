/**
 * The carrier in 3D, from the same deck geometry the 2D trap sheet and the
 * engine use (lib/carrierDeck.ts), so the wires a pilot sees in the 3D
 * debrief are where the engine counted them.
 *
 * Model frame: +Y forward, +X starboard, Z up from the waterline, origin at
 * the ship's reference point (the position DCS / Tacview report).
 * The flight deck is the real outline extruded, with a painted texture: the
 * landing area and its edge lines, the centreline, the foul line, the four
 * wires, the bow catapults and the hull number; the island, the lens
 * (IFLOLS) and the LSO platform sit where the deck plan puts them.
 */
import * as THREE from 'three'
import { landingPointFromStern, shipToModel, type DeckGeom } from '../../lib/carrierDeck'
import { MATS } from './models'

type P = [number, number]
const D2R = Math.PI / 180

/** Pixels per metre of the deck texture. */
const PPM = 6

/** The landing area's centreline in ship coordinates (f fwd of stern, r right of keel) at `d` metres from the stern. */
function onLanding(g: DeckGeom, d: number, off = 0): P {
  const a = g.angle_deg * D2R
  // along the landing direction, then `off` to its right
  return [d * Math.cos(a) - off * Math.sin(a), g.stern_offset_m + d * Math.sin(a) + off * Math.cos(a)]
}

function deckTexture(g: DeckGeom, hullNo: string | null, minX: number, maxX: number, minY: number, maxY: number): THREE.CanvasTexture {
  const W = Math.ceil((maxX - minX) * PPM), H = Math.ceil((maxY - minY) * PPM)
  const cv = document.createElement('canvas')
  cv.width = W
  cv.height = H
  const c = cv.getContext('2d')!
  // model (x right, y fwd) → canvas px
  const X = (x: number) => (x - minX) * PPM
  const Y = (y: number) => (maxY - y) * PPM
  const S = (p: P) => { const [y, x] = shipToModel(g, p[0], p[1]); return [X(x), Y(y)] as const }
  c.fillStyle = '#3e444b'
  c.fillRect(0, 0, W, H)
  // the landing area, a shade darker from rubber
  const hw = g.landing_half_w_m
  const L = g.landing_len_m
  const quad = [onLanding(g, 0, -hw), onLanding(g, L, -hw), onLanding(g, L, hw), onLanding(g, 0, hw)].map(S)
  c.fillStyle = '#33383e'
  c.beginPath(); quad.forEach(([x, y], i) => (i ? c.lineTo(x, y) : c.moveTo(x, y))); c.closePath(); c.fill()
  const line = (a: P, b: P, color: string, w: number, dash: number[] = []) => {
    const [ax, ay] = S(a), [bx, by] = S(b)
    c.strokeStyle = color; c.lineWidth = w * PPM; c.setLineDash(dash.map(d => d * PPM))
    c.beginPath(); c.moveTo(ax, ay); c.lineTo(bx, by); c.stroke()
  }
  // edge lines, centreline, foul line
  line(onLanding(g, 0, -hw), onLanding(g, L, -hw), '#e8e8e8', 0.45)
  line(onLanding(g, 0, hw), onLanding(g, L, hw), '#e8e8e8', 0.45)
  line(onLanding(g, 3, 0), onLanding(g, L, 0), '#e8e8e8', 0.35, [4, 4])
  if (g.angle_deg !== 0) line(onLanding(g, -2, hw + 5), onLanding(g, L, hw + 5), '#d9442f', 0.35, [1.5, 1.5])
  // elevators: yellow safety outlines
  for (const e of g.elevators) {
    c.strokeStyle = '#c7a73c'; c.lineWidth = 0.35 * PPM; c.setLineDash([])
    c.beginPath(); e.forEach((q, i) => { const [x, y] = S(q); if (i) c.lineTo(x, y); else c.moveTo(x, y) }); c.closePath(); c.stroke()
  }
  // ski-jump take-off runs and helicopter spots
  g.runs.forEach(([a, b]) => line(a, b, '#e8e8e8', 0.3, [3, 3]))
  g.spots.forEach((q, i) => {
    const [x, y] = S(q)
    c.strokeStyle = '#e8e8e8'; c.lineWidth = 0.35 * PPM; c.setLineDash([])
    c.beginPath(); c.arc(x, y, 6 * PPM, 0, Math.PI * 2); c.stroke()
    c.fillStyle = '#e8e8e8'; c.font = `bold ${5 * PPM}px "Archivo", sans-serif`; c.textAlign = 'center'; c.textBaseline = 'middle'
    c.fillText(String(i + 1), x, y)
  })
  // ramp stripes
  for (let i = 0; i < 9; i++) {
    const o = -hw + (i * 2 * hw) / 8
    line(onLanding(g, 0, o), onLanding(g, 2.5, o), i % 2 ? '#111' : '#e8e8e8', 0.9)
  }
  // wires (drawn here; the wires themselves are meshes)
  g.wires_m.forEach(d => line(onLanding(g, d, -hw - 1), onLanding(g, d, hw + 1), '#c7a73c', 0.25))
  // bow catapults
  g.cats.forEach(([a, b]) => line(a, b, '#1e2226', 0.7))
  g.cats.forEach(([a, b]) => line(a, b, '#8a8f95', 0.12))
  // the hull number on the bow, reading from aft
  if (hullNo && !g.spots.length) {
    const [x, y] = S([g.length_m * 0.86, -1])
    c.save()
    c.translate(x, y)
    c.fillStyle = '#e8e8e8'
    c.font = `bold ${16 * PPM}px "Archivo", sans-serif`
    c.textAlign = 'center'
    c.textBaseline = 'middle'
    c.fillText(hullNo, 0, 0)
    c.restore()
  }
  const t = new THREE.CanvasTexture(cv)
  t.colorSpace = THREE.SRGBColorSpace
  t.anisotropy = 8
  t.wrapS = t.wrapT = THREE.ClampToEdgeWrapping
  t.repeat.set(1 / (maxX - minX), 1 / (maxY - minY))
  t.offset.set(-minX / (maxX - minX), -minY / (maxY - minY))
  return t
}

/** "CVN_73" → "73" for the deck. */
export function hullNumber(name: string | undefined): string | null {
  const m = /CVN[-_ ]?(\d{2})/i.exec(name ?? '')
  if (m) return m[1]
  if (/stennis/i.test(name ?? '')) return '74'
  if (/forrestal/i.test(name ?? '')) return '59'
  return null
}

/** Where the LSO stands and the lens is, in model coordinates (metres fwd / right of the reference point, height). */
export function deckSpots(g: DeckGeom) {
  const at = (p: P | null) => (p ? (([y, x]) => [x, y, g.deck_height_m] as [number, number, number])(shipToModel(g, p[0], p[1])) : null)
  return { lso: at(g.lso), lens: at(g.lens) }
}

export function carrier(g: DeckGeom, name: string | undefined): THREE.Group {
  const grp = new THREE.Group()
  grp.userData.length = g.length_m
  const H = g.deck_height_m
  // deck outline in model coordinates (shape x = right, shape y = forward)
  const pts = g.outline.map(([f, r]) => { const [y, x] = shipToModel(g, f, r); return new THREE.Vector2(x, y) })
  const xs = pts.map(p => p.x), ys = pts.map(p => p.y)
  const [minX, maxX, minY, maxY] = [Math.min(...xs), Math.max(...xs), Math.min(...ys), Math.max(...ys)]
  const shape = new THREE.Shape(pts)

  const slab = new THREE.Mesh(new THREE.ExtrudeGeometry(shape, { depth: 1.4, bevelEnabled: false }), MATS.navyDark)
  slab.position.z = H - 1.4
  grp.add(slab)
  const top = new THREE.Mesh(
    new THREE.ShapeGeometry(shape),
    new THREE.MeshStandardMaterial({ map: deckTexture(g, hullNumber(name), minX, maxX, minY, maxY), roughness: 0.95, metalness: 0 }),
  )
  top.position.z = H + 0.02
  grp.add(top)

  // the hull: the plan's waterline, up to the gallery deck under the flight deck overhang
  const wl = g.waterline.map(([f, r]) => { const [y, x] = shipToModel(g, f, r); return new THREE.Vector2(x, y) })
  const hullShape = new THREE.Shape(wl)
  const hull = new THREE.Mesh(new THREE.ExtrudeGeometry(hullShape, { depth: H - 1.4 + 3, bevelEnabled: false }), MATS.hull)
  hull.position.z = -3
  grp.add(hull)
  const boot = new THREE.Mesh(new THREE.ExtrudeGeometry(hullShape, { depth: 3.2, bevelEnabled: false }), MATS.hullLow)
  boot.position.z = -3.1
  boot.scale.set(1.005, 1.002, 1)
  grp.add(boot)

  // a ski jump: the bow ramp rising to about 6 m
  if (g.ski_jump) {
    const [f0, f1] = g.ski_jump
    const [y0] = shipToModel(g, f0, 0), [y1] = shipToModel(g, f1, 0)
    const prof = new THREE.Shape([new THREE.Vector2(y0, 0), new THREE.Vector2(y1, 0), new THREE.Vector2(y1, 6), new THREE.Vector2(y0 + (y1 - y0) * 0.55, 1.6)])
    const ramp = new THREE.Mesh(new THREE.ExtrudeGeometry(prof, { depth: 26, bevelEnabled: false }), MATS.navyDark)
    // profile drawn in (forward, up); extrude across the deck
    // three applies Y, then X, then Z with this order: profile up -> Z, extrusion -> +X
    ramp.rotation.order = 'ZXY'
    ramp.rotation.set(Math.PI / 2, 0, Math.PI / 2)
    ramp.position.set(-13, 0, H + 0.03)
    grp.add(ramp)
  }

  // island: the base block from its footprint, the upper block, the mast
  const isl = g.island.map(([f, r]) => { const [y, x] = shipToModel(g, f, r); return new THREE.Vector2(x, y) })
  const islandBase = new THREE.Mesh(new THREE.ExtrudeGeometry(new THREE.Shape(isl), { depth: 14, bevelEnabled: false }), MATS.island)
  islandBase.position.z = H
  grp.add(islandBase)
  if (g.island_top.length > 2) {
    const top = g.island_top.map(([f, r]) => { const [y, x] = shipToModel(g, f, r); return new THREE.Vector2(x, y) })
    const upper = new THREE.Mesh(new THREE.ExtrudeGeometry(new THREE.Shape(top), { depth: 7, bevelEnabled: false }), MATS.island)
    upper.position.z = H + 14
    grp.add(upper)
  }
  const ix = isl.map(p => p.x), iy = isl.map(p => p.y)
  const ic = new THREE.Vector2((Math.min(...ix) + Math.max(...ix)) / 2, (Math.min(...iy) + Math.max(...iy)) / 2)
  const mast = new THREE.Mesh(new THREE.CylinderGeometry(0.4, 0.7, 16, 6), MATS.island)
  mast.rotation.x = Math.PI / 2
  mast.position.set(ic.x, ic.y, H + 21 + 8)
  grp.add(mast)

  // wires: steel across the landing area, a hand's breadth off the deck
  const hw = g.landing_half_w_m + 1
  const wireMat = new THREE.MeshStandardMaterial({ color: '#c9c3a8', metalness: 0.8, roughness: 0.3 })
  for (const d of g.wires_m) {
    const [ay, ax] = shipToModel(g, ...onLanding(g, d, -hw))
    const [by, bx] = shipToModel(g, ...onLanding(g, d, hw))
    const w = new THREE.Mesh(new THREE.CylinderGeometry(0.035, 0.035, Math.hypot(bx - ax, by - ay), 5), wireMat)
    w.position.set((ax + bx) / 2, (ay + by) / 2, H + 0.12)
    w.rotation.z = Math.atan2(by - ay, bx - ax) - Math.PI / 2
    grp.add(w)
  }

  // the lens: a box on the port side with the ball and the datum lights
  const spots = deckSpots(g)
  if (spots.lens) {
    const [x, y, z] = spots.lens
    const box = new THREE.Mesh(new THREE.BoxGeometry(0.8, 1.2, 4.2), MATS.navyDark)
    box.position.set(x, y, z + 2.4)
    grp.add(box)
    const ball = new THREE.Mesh(new THREE.BoxGeometry(0.85, 1.3, 0.5), MATS.lensAmber)
    ball.position.set(x, y + 0.05, z + 2.4)
    grp.add(ball)
    for (const s of [-1, 1]) {
      const datum = new THREE.Mesh(new THREE.BoxGeometry(0.5, 2.6, 0.3), MATS.lensGreen)
      datum.position.set(x, y + s * 1.9, z + 2.4)
      grp.add(datum)
    }
  }
  if (spots.lso) {
    const [x, y, z] = spots.lso
    const plat = new THREE.Mesh(new THREE.BoxGeometry(4, 6, 0.4), MATS.navyDark)
    plat.position.set(x - 1.5, y, z - 0.6)
    grp.add(plat)
  }
  grp.userData.landingFromStern = landingPointFromStern(g)
  return grp
}
