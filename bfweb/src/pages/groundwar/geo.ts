// Flat-earth helpers. Everything on this screen is within a few hundred km,
// and these only decide "which is nearer", lay out vehicles, or interpolate a
// few metres between frames.
import type { LatLon } from '../../api'

const M_PER_DEG_LAT = 110_574

export function mPerDegLon(lat: number): number {
  return 111_320 * Math.cos((lat * Math.PI) / 180)
}

/** Metres between two points. */
export function dist(a: LatLon, b: LatLon): number {
  const dx = (a[1] - b[1]) * mPerDegLon((a[0] + b[0]) / 2)
  const dy = (a[0] - b[0]) * M_PER_DEG_LAT
  return Math.sqrt(dx * dx + dy * dy)
}

export const km = (a: LatLon, b: LatLon): number => dist(a, b) / 1000

/** Degrees true from a to b. */
export function bearing(a: LatLon, b: LatLon): number {
  const dx = (b[1] - a[1]) * mPerDegLon((a[0] + b[0]) / 2)
  const dy = (b[0] - a[0]) * M_PER_DEG_LAT
  return ((Math.atan2(dx, dy) * 180) / Math.PI + 360) % 360
}

/** The point `m` metres from p on heading `hdg` (degrees true). */
export function offset(p: LatLon, hdg: number, m: number): LatLon {
  const r = (hdg * Math.PI) / 180
  return [p[0] + (Math.cos(r) * m) / M_PER_DEG_LAT, p[1] + (Math.sin(r) * m) / mPerDegLon(p[0])]
}

/** The point east/north metres from p. */
export function offsetEN(p: LatLon, east: number, north: number): LatLon {
  return [p[0] + north / M_PER_DEG_LAT, p[1] + east / mPerDegLon(p[0])]
}

export function lerp(a: LatLon, b: LatLon, t: number): LatLon {
  return [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]
}

/** Shortest-way interpolation between two headings. */
export function lerpHeading(a: number, b: number, t: number): number {
  const d = ((b - a + 540) % 360) - 180
  return (a + d * t + 360) % 360
}

/** Length of a polyline, metres. */
export function pathLength(p: LatLon[]): number {
  let s = 0
  for (let i = 1; i < p.length; i++) s += dist(p[i - 1], p[i])
  return s
}

/** The point `m` metres along a polyline, with the heading of that leg. */
export function along(p: LatLon[], m: number): { pos: LatLon; hdg: number } {
  if (p.length === 0) return { pos: [0, 0], hdg: 0 }
  if (p.length === 1 || m <= 0) return { pos: p[0], hdg: p.length > 1 ? bearing(p[0], p[1]) : 0 }
  let left = m
  for (let i = 1; i < p.length; i++) {
    const d = dist(p[i - 1], p[i])
    if (left <= d) return { pos: lerp(p[i - 1], p[i], d > 0 ? left / d : 0), hdg: bearing(p[i - 1], p[i]) }
    left -= d
  }
  const n = p.length
  return { pos: p[n - 1], hdg: bearing(p[n - 2], p[n - 1]) }
}

export const toLngLat = (p: LatLon): [number, number] => [p[1], p[0]]

export function fmtAge(secs: number): string {
  const m = Math.round(secs / 60)
  if (m < 1) return `${Math.max(1, Math.round(secs))} S`
  if (m < 60) return `${m} MIN`
  return `${Math.floor(m / 60)} H ${m % 60} MIN`
}

export function fmtZulu(unix: number): string {
  const d = new Date(unix * 1000)
  const p = (n: number) => String(n).padStart(2, '0')
  return `${p(d.getUTCHours())}:${p(d.getUTCMinutes())}:${p(d.getUTCSeconds())}Z`
}
