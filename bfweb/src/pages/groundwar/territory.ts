// Zones of control for the command map: each land base's Voronoi cell (the
// ground nearer to it than to any other base), cut to a circle so a lone
// base doesn't claim the whole map.
import type { Feature, FeatureCollection, Polygon } from 'geojson'
import type { GroundObjective, LatLon } from '../../api'

/** How far a base's zone of control reaches, km. */
const TERRITORY_KM = 40
const SEGMENTS = 28

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

/** Each land base's zone of control: its Voronoi cell, cut to a circle. */
export function territoryCells(objs: GroundObjective[]): FeatureCollection<Polygon> {
  const land = objs.filter((o) => o.kind !== 'carrier' && o.kind !== 'naval')
  if (land.length === 0) return { type: 'FeatureCollection', features: [] }
  const lat0 = land.reduce((s, o) => s + o.pos[0], 0) / land.length
  const lon0 = land.reduce((s, o) => s + o.pos[1], 0) / land.length
  const kx = 111_320 * Math.cos((lat0 * Math.PI) / 180)
  const ky = 110_540
  const toXY = (p: LatLon): XY => [(p[1] - lon0) * kx, (p[0] - lat0) * ky]
  const toLL = (q: XY): [number, number] => [q[0] / kx + lon0, q[1] / ky + lat0]
  const r = TERRITORY_KM * 1000
  const sites = land.map((o) => toXY(o.pos))
  const features: Feature<Polygon>[] = []
  land.forEach((o, i) => {
    const s = sites[i]
    let poly: XY[] = Array.from({ length: SEGMENTS }, (_, k) => {
      const a = (k / SEGMENTS) * Math.PI * 2
      return [s[0] + Math.cos(a) * r, s[1] + Math.sin(a) * r]
    })
    for (let j = 0; j < sites.length && poly.length > 2; j++) {
      if (j === i) continue
      const t = sites[j]
      const dx = t[0] - s[0]
      const dy = t[1] - s[1]
      if (dx * dx + dy * dy > 4 * r * r) continue
      poly = clip(poly, [(s[0] + t[0]) / 2, (s[1] + t[1]) / 2], [dx, dy])
    }
    if (poly.length < 3) return
    const ring = poly.map(toLL)
    ring.push(ring[0])
    features.push({ type: 'Feature', properties: { owner: o.owner }, geometry: { type: 'Polygon', coordinates: [ring] } })
  })
  return { type: 'FeatureCollection', features }
}

