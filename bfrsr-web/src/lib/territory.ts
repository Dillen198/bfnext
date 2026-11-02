// The dashboard plot's territory: the ground shared out between the bases as
// a Voronoi diagram (each base holds what is nearer to it than to any other
// base), shaded by owner, with the front drawn where one side's ground meets
// the other's. Carrier groups hold no ground.
//
// Where we have a coastline for the theatre the ground stops at the shore and
// so does the front; anywhere else it runs out to a box around the bases.
import type { LatLon } from '../api'

/** How far past the outermost bases the ground runs, km: past the edge of
 *  the plot, so the shading fills it the way the ground does. */
const PAD_KM = 500

export interface TerritorySite {
  lat: number
  lon: number
  owner: string
  kind: string
}

export interface TerritoryMap {
  /** Each base's ground as a ring of [lat, lon], with its owner. */
  cells: { owner: string; ring: LatLon[] }[]
  /** Where ground of two different owners meets, on land. */
  front: [LatLon, LatLon][]
}

// The Caucasus' land, 31.5-50 E by 37.5-48.5 N: everything but the Black Sea,
// the Sea of Azov and the Caspian, whose coasts are traced by hand
// (approximate; the Caucasus stretch is the Southern Flank briefing map's).
const CAUCASUS_LAND: LatLon[] = [
  [48.5, 31.5], [46.62, 31.5],
  // Ukraine, then Crimea round to Kerch
  [46.5, 32.0], [46.25, 32.5], [46.12, 32.9], [46.07, 33.4], [46.1, 33.62], [45.9, 33.5], [45.85, 33.0],
  [45.6, 32.75], [45.4, 32.5], [45.19, 33.37], [44.95, 33.6], [44.6, 33.45], [44.4, 33.73], [44.49, 34.17],
  [44.67, 34.41], [44.84, 34.97], [45.03, 35.4], [45.0, 35.8], [45.08, 36.2], [45.2, 36.45], [45.35, 36.47],
  // the Sea of Azov: Crimea's shore, the Arabat spit, then round to Taman
  [45.44, 36.3], [45.47, 35.85], [45.36, 35.45], [45.6, 35.2], [45.95, 34.97], [46.17, 34.82], [46.35, 35.35],
  [46.73, 36.34], [46.75, 36.8], [46.95, 37.3], [47.08, 37.6], [47.1, 38.2], [47.2, 38.9], [47.2, 39.25],
  [47.05, 39.1], [46.9, 38.6], [46.7, 38.27], [46.5, 38.5], [46.05, 38.17],
  [45.75, 37.72], [45.6, 37.66], [45.4, 37.55], [45.29, 37.4], [45.31, 37.2], [45.37, 37.02], [45.42, 36.86],
  [45.37, 36.67], [45.42, 36.5],
  // the Black Sea: Taman, the Caucasus coast and Turkey
  [45.22, 36.5], [45.18, 36.68], [45.1, 36.85], [45.04, 37.1], [44.96, 37.25], [44.89, 37.31], [44.8, 37.41],
  [44.71, 37.57], [44.66, 37.74], [44.72, 37.79], [44.66, 37.92], [44.56, 38.06], [44.46, 38.3], [44.37, 38.53],
  [44.31, 38.71], [44.24, 38.86], [44.1, 39.07], [43.98, 39.22], [43.9, 39.34], [43.76, 39.52], [43.66, 39.65],
  [43.58, 39.72], [43.49, 39.85], [43.42, 39.95], [43.38, 40.01], [43.31, 40.18], [43.26, 40.27], [43.18, 40.3],
  [43.15, 40.37], [43.12, 40.55], [43.09, 40.76], [43.03, 40.94], [42.99, 41.03], [42.93, 41.13], [42.83, 41.3],
  [42.71, 41.46], [42.55, 41.53], [42.39, 41.56], [42.25, 41.62], [42.15, 41.66], [42.03, 41.73], [41.9, 41.77],
  [41.82, 41.77], [41.72, 41.72], [41.66, 41.64], [41.62, 41.62], [41.56, 41.57], [41.52, 41.55], [41.4, 41.42],
  [41.35, 41.3], [41.25, 41.1], [41.18, 40.9], [41.08, 40.66], [41.03, 40.52], [40.98, 40.2], [41.0, 39.72],
  [40.96, 39.2], [40.92, 38.6], [40.96, 38.0], [41.08, 37.4], [41.2, 36.95], [41.3, 36.5],
  [41.45, 36.15], [41.74, 35.95], [42.03, 35.15], [42.1, 34.93], [41.98, 34.3], [41.98, 33.76], [41.9, 33.0],
  [41.75, 32.39], [41.6, 32.05], [41.45, 31.79], [41.33, 31.5],
  // round the south, then the Caspian's west shore
  [37.5, 31.5], [37.5, 48.9], [37.8, 48.98], [38.43, 48.88], [38.75, 48.85], [39.15, 48.95], [39.33, 49.4],
  [39.95, 49.42], [40.36, 49.86], [40.55, 49.97], [40.59, 49.67], [41.08, 49.12], [41.6, 48.7], [42.06, 48.3],
  [42.56, 47.87], [42.98, 47.5], [43.27, 47.55], [43.65, 47.55], [43.9, 47.4], [44.3, 47.0], [44.8, 46.95],
  [45.39, 47.37], [45.8, 47.6], [46.0, 48.3], [46.25, 49.0], [46.6, 49.6], [46.75, 50.0],
  [48.5, 50.0],
]
const LAND: { lat: [number, number]; lon: [number, number]; ring: LatLon[] }[] = [
  { lat: [40, 47], lon: [36.5, 48], ring: CAUCASUS_LAND },
]

type XY = [number, number]

/** Clip a convex polygon to the half-plane (p - m) . n <= 0. */
function clip(poly: XY[], m: XY, n: XY): XY[] {
  const side = (p: XY) => (p[0] - m[0]) * n[0] + (p[1] - m[1]) * n[1]
  const out: XY[] = []
  for (let i = 0; i < poly.length; i++) {
    const a = poly[i]
    const b = poly[(i + 1) % poly.length]
    const sa = side(a)
    const sb = side(b)
    if (sa <= 0) out.push(a)
    if ((sa <= 0) !== (sb <= 0)) {
      const t = sa / (sa - sb)
      out.push([a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t])
    }
  }
  return out
}

/** Clip a simple polygon that may be concave (the coast) to the same
 *  half-plane, as separate pieces. Clipping it like a convex one leaves the
 *  pieces joined by zero-width slivers along the cut, which a browser still
 *  draws as faint lines. */
export function clipPieces(poly: XY[], m: XY, n: XY): XY[][] {
  const side = (p: XY) => (p[0] - m[0]) * n[0] + (p[1] - m[1]) * n[1]
  const along = (p: XY) => -p[0] * n[1] + p[1] * n[0]
  // The inside runs of the boundary, each from where it comes in across the
  // line to where it goes out again.
  const chains: XY[][] = []
  let cur: XY[] | null = null
  let start = -1
  for (let i = 0; i < poly.length; i++) {
    if (side(poly[i]) > 0 && side(poly[(i + 1) % poly.length]) <= 0) { start = i; break }
  }
  if (start < 0) return poly.every((p) => side(p) <= 0) ? [poly] : []
  for (let k = 0; k < poly.length; k++) {
    const i = (start + k) % poly.length
    const a = poly[i]
    const b = poly[(i + 1) % poly.length]
    const sa = side(a)
    const sb = side(b)
    if (sa <= 0 && cur) cur.push(a)
    if ((sa <= 0) !== (sb <= 0)) {
      const t = sa / (sa - sb)
      const x: XY = [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]
      if (sb <= 0) cur = [x]
      else if (cur) { cur.push(x); chains.push(cur); cur = null }
    }
  }
  // Along the line, the crossings pair up into the stretches inside the
  // polygon; each stretch joins one run's way out to some run's way in.
  const ends = chains.flatMap((c, ci) => [
    { ci, out: false, at: along(c[0]) },
    { ci, out: true, at: along(c[c.length - 1]) },
  ]).sort((p, q) => p.at - q.at)
  const next = new Map<number, number>()
  for (let k = 0; k + 1 < ends.length; k += 2) {
    const [p, q] = [ends[k], ends[k + 1]]
    if (p.out === q.out) return [chains.flat()]
    if (p.out) next.set(p.ci, q.ci)
    else next.set(q.ci, p.ci)
  }
  const used = new Set<number>()
  const pieces: XY[][] = []
  for (let c = 0; c < chains.length; c++) {
    if (used.has(c)) continue
    const ring: XY[] = []
    for (let at: number | undefined = c; at !== undefined && !used.has(at); at = next.get(at)) {
      used.add(at)
      ring.push(...chains[at])
    }
    if (ring.length >= 3) pieces.push(ring)
  }
  return pieces
}

function inside(p: XY, poly: XY[]): boolean {
  let hit = false
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const [xi, yi] = poly[i]
    const [xj, yj] = poly[j]
    if ((yi > p[1]) !== (yj > p[1]) && p[0] < ((xj - xi) * (p[1] - yi)) / (yj - yi) + xi) hit = !hit
  }
  return hit
}

/** The pieces of segment a-b that lie inside `poly`. */
function onLand(a: XY, b: XY, poly: XY[]): [XY, XY][] {
  const dx = b[0] - a[0]
  const dy = b[1] - a[1]
  const ts = [0, 1]
  for (let i = 0; i < poly.length; i++) {
    const p = poly[i]
    const q = poly[(i + 1) % poly.length]
    const ex = q[0] - p[0]
    const ey = q[1] - p[1]
    const den = dx * ey - dy * ex
    if (den === 0) continue
    const t = ((p[0] - a[0]) * ey - (p[1] - a[1]) * ex) / den
    const u = ((p[0] - a[0]) * dy - (p[1] - a[1]) * dx) / den
    if (t > 0 && t < 1 && u >= 0 && u <= 1) ts.push(t)
  }
  ts.sort((x, y) => x - y)
  const at = (t: number): XY => [a[0] + dx * t, a[1] + dy * t]
  const out: [XY, XY][] = []
  for (let k = 0; k + 1 < ts.length; k++) {
    if (ts[k + 1] - ts[k] < 1e-9 || !inside(at((ts[k] + ts[k + 1]) / 2), poly)) continue
    const last = out[out.length - 1]
    const from = at(ts[k])
    if (last && last[1][0] === from[0] && last[1][1] === from[1]) last[1] = at(ts[k + 1])
    else out.push([from, at(ts[k + 1])])
  }
  return out
}

/** The ground each base holds and the front between the sides. */
export function territoryMap(sites: TerritorySite[]): TerritoryMap {
  const land = sites.filter((o) => !/carrier/i.test(o.kind))
  if (land.length === 0) return { cells: [], front: [] }
  const lat0 = land.reduce((s, o) => s + o.lat, 0) / land.length
  const lon0 = land.reduce((s, o) => s + o.lon, 0) / land.length
  const kx = 111_320 * Math.cos((lat0 * Math.PI) / 180)
  const ky = 110_540
  const toXY = (p: LatLon): XY => [(p[1] - lon0) * kx, (p[0] - lat0) * ky]
  const toLL = (q: XY): LatLon => [q[1] / ky + lat0, q[0] / kx + lon0]

  const xy = land.map((o) => toXY([o.lat, o.lon]))
  const pad = PAD_KM * 1000
  const x0 = Math.min(...xy.map((p) => p[0])) - pad
  const x1 = Math.max(...xy.map((p) => p[0])) + pad
  const y0 = Math.min(...xy.map((p) => p[1])) - pad
  const y1 = Math.max(...xy.map((p) => p[1])) + pad
  const box: XY[] = [[x0, y0], [x1, y0], [x1, y1], [x0, y1]]
  const shore = LAND.find((l) => lat0 >= l.lat[0] && lat0 <= l.lat[1] && lon0 >= l.lon[0] && lon0 <= l.lon[1])
  const coast = shore?.ring.map(toXY) ?? null

  // Each base's whole cell (convex), then its ground: the cell cut to land.
  const cells = xy.map((s, i) => {
    let poly = box
    for (let j = 0; j < xy.length && poly.length > 2; j++) {
      if (j === i) continue
      const t = xy[j]
      poly = clip(poly, [(s[0] + t[0]) / 2, (s[1] + t[1]) / 2], [t[0] - s[0], t[1] - s[1]])
    }
    return poly
  })
  const out: TerritoryMap = { cells: [], front: [] }
  cells.forEach((cell, i) => {
    if (cell.length < 3) return
    let ground = [cell]
    if (coast) {
      ground = [coast]
      for (let k = 0; k < cell.length && ground.length > 0; k++) {
        const a = cell[k]
        const b = cell[(k + 1) % cell.length]
        // The outward normal of edge a-b: the side away from the base.
        let n: XY = [b[1] - a[1], a[0] - b[0]]
        if ((xy[i][0] - a[0]) * n[0] + (xy[i][1] - a[1]) * n[1] > 0) n = [-n[0], -n[1]]
        ground = ground.flatMap((g) => clipPieces(g, a, n))
      }
    }
    for (const g of ground) out.cells.push({ owner: land[i].owner, ring: g.map(toLL) })
  })

  // The front: the edge two cells share, where their owners differ. Both of
  // its ends are vertices of the cell lying on the bases' bisector.
  for (let i = 0; i < xy.length; i++) {
    for (let j = i + 1; j < xy.length; j++) {
      if (land[i].owner === land[j].owner || cells[i].length < 3) continue
      const s = xy[i]
      const t = xy[j]
      const m: XY = [(s[0] + t[0]) / 2, (s[1] + t[1]) / 2]
      const len = Math.hypot(t[0] - s[0], t[1] - s[1])
      if (len === 0) continue
      const on = cells[i].filter((p) => Math.abs((p[0] - m[0]) * (t[0] - s[0]) + (p[1] - m[1]) * (t[1] - s[1])) / len < 1)
      if (on.length < 2) continue
      const a = on[0]
      const b = on.reduce((far, p) => (Math.hypot(p[0] - a[0], p[1] - a[1]) > Math.hypot(far[0] - a[0], far[1] - a[1]) ? p : far))
      if (Math.hypot(b[0] - a[0], b[1] - a[1]) < 1) continue
      for (const [p, q] of coast ? onLand(a, b, coast) : [[a, b] as [XY, XY]]) out.front.push([toLL(p), toLL(q)])
    }
  }
  return out
}
