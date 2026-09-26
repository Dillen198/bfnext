/**
 * Small-area geodesy for range geometry. Everything the site draws spans a
 * few tens of kilometres at most, so a local tangent plane (equirectangular
 * about an origin) is well inside a metre of the truth and keeps the maths
 * readable. Distances between arbitrary points use haversine.
 */

export const EARTH_R = 6_371_008.8
export const NM = 1852
export const FT = 0.3048
export const KT = 0.514444
const D2R = Math.PI / 180

export interface LatLon {
  lat: number
  lon: number
}

export const rad = (d: number) => d * D2R
export const deg = (r: number) => r / D2R

/** Normalise to [0, 360). */
export function norm360(d: number): number {
  return ((d % 360) + 360) % 360
}

/** Normalise to (-180, 180]. */
export function norm180(d: number): number {
  const n = norm360(d)
  return n > 180 ? n - 360 : n
}

/** Local north/east metres of `p` relative to `origin`. */
export function toLocal(origin: LatLon, p: LatLon): { north: number; east: number } {
  return {
    north: rad(p.lat - origin.lat) * EARTH_R,
    east: rad(p.lon - origin.lon) * EARTH_R * Math.cos(rad(origin.lat)),
  }
}

/** The point `north`/`east` metres from `origin`. */
export function fromLocal(origin: LatLon, north: number, east: number): LatLon {
  return {
    lat: origin.lat + deg(north / EARTH_R),
    lon: origin.lon + deg(east / (EARTH_R * Math.cos(rad(origin.lat)))),
  }
}

/** Point at `dist` metres on true bearing `brg` from `p`. */
export function destination(p: LatLon, brg: number, dist: number): LatLon {
  return fromLocal(p, Math.cos(rad(brg)) * dist, Math.sin(rad(brg)) * dist)
}

export function distanceM(a: LatLon, b: LatLon): number {
  const dLat = rad(b.lat - a.lat)
  const dLon = rad(b.lon - a.lon)
  const h =
    Math.sin(dLat / 2) ** 2 + Math.cos(rad(a.lat)) * Math.cos(rad(b.lat)) * Math.sin(dLon / 2) ** 2
  return 2 * EARTH_R * Math.asin(Math.min(1, Math.sqrt(h)))
}

export function bearingDeg(a: LatLon, b: LatLon): number {
  const y = Math.sin(rad(b.lon - a.lon)) * Math.cos(rad(b.lat))
  const x =
    Math.cos(rad(a.lat)) * Math.sin(rad(b.lat)) -
    Math.sin(rad(a.lat)) * Math.cos(rad(b.lat)) * Math.cos(rad(b.lon - a.lon))
  return norm360(deg(Math.atan2(y, x)))
}

/** A closed polygon ring, GeoJSON order ([lon, lat]). */
export function circleRing(c: LatLon, radiusM: number, steps = 64): [number, number][] {
  const ring: [number, number][] = []
  for (let i = 0; i <= steps; i++) {
    const p = destination(c, (i / steps) * 360, radiusM)
    ring.push([p.lon, p.lat])
  }
  return ring
}

/** Bounds [[w, s], [e, n]] around a set of points, padded by `padM`. */
export function boundsOf(pts: LatLon[], padM = 0): [[number, number], [number, number]] | null {
  if (!pts.length) return null
  let s = Infinity, n = -Infinity, w = Infinity, e = -Infinity
  for (const p of pts) {
    s = Math.min(s, p.lat); n = Math.max(n, p.lat)
    w = Math.min(w, p.lon); e = Math.max(e, p.lon)
  }
  if (padM > 0) {
    const sw = fromLocal({ lat: s, lon: w }, -padM, -padM)
    const ne = fromLocal({ lat: n, lon: e }, padM, padM)
    return [[sw.lon, sw.lat], [ne.lon, ne.lat]]
  }
  return [[w, s], [e, n]]
}
