/**
 * Coordinates the way they go into a cockpit: degrees-decimal-minutes (most
 * jets' UFC), degrees-minutes-seconds, decimal, and MGRS (A-10C, AH-64,
 * JTAC talk-ons). WGS84 throughout, the datum DCS uses for its F10 readout.
 */
import type { LatLon } from './geo'

export type CoordFormat = 'ddm' | 'dms' | 'dec' | 'mgrs'

export const COORD_FORMATS: { v: CoordFormat; label: string }[] = [
  { v: 'ddm', label: 'DD°MM.mmm' },
  { v: 'dms', label: 'DD°MM\'SS"' },
  { v: 'mgrs', label: 'MGRS' },
  { v: 'dec', label: 'Decimal' },
]

const pad = (n: number, w: number) => String(n).padStart(w, '0')

function ddm(v: number, pos: string, neg: string, degW: number): string {
  const h = v < 0 ? neg : pos
  const a = Math.abs(v)
  let d = Math.floor(a)
  let m = Math.round((a - d) * 60 * 1000) / 1000
  if (m >= 60) { d += 1; m = 0 }
  return `${h}${pad(d, degW)}°${m.toFixed(3).padStart(6, '0')}'`
}

function dms(v: number, pos: string, neg: string, degW: number): string {
  const h = v < 0 ? neg : pos
  const a = Math.abs(v)
  let d = Math.floor(a)
  let m = Math.floor((a - d) * 60)
  let s = Math.round(((a - d) * 60 - m) * 60 * 10) / 10
  if (s >= 60) { s = 0; m += 1 }
  if (m >= 60) { m = 0; d += 1 }
  return `${h}${pad(d, degW)}°${pad(m, 2)}'${s.toFixed(1).padStart(4, '0')}"`
}

// ─── UTM / MGRS ────────────────────────────────────────────────────────────

const A = 6378137
const F = 1 / 298.257223563
const K0 = 0.9996
const E2 = F * (2 - F)
const EP2 = E2 / (1 - E2)
const BANDS = 'CDEFGHJKLMNPQRSTUVWXX'
const COLS = ['ABCDEFGH', 'JKLMNPQR', 'STUVWXYZ']
const ROWS = 'ABCDEFGHJKLMNPQRSTUV'

export interface Utm {
  zone: number
  band: string
  easting: number
  northing: number
}

export function toUtm({ lat, lon }: LatLon): Utm {
  let zone = Math.floor((lon + 180) / 6) + 1
  if (zone > 60) zone = 60
  // the two exceptions: south-west Norway and Svalbard
  if (lat >= 56 && lat < 64 && lon >= 3 && lon < 12) zone = 32
  if (lat >= 72 && lat < 84) {
    if (lon >= 0 && lon < 9) zone = 31
    else if (lon >= 9 && lon < 21) zone = 33
    else if (lon >= 21 && lon < 33) zone = 35
    else if (lon >= 33 && lon < 42) zone = 37
  }
  const phi = (lat * Math.PI) / 180
  const lam = (lon * Math.PI) / 180
  const lam0 = (((zone - 1) * 6 - 180 + 3) * Math.PI) / 180
  const sin = Math.sin(phi), cos = Math.cos(phi), tan = Math.tan(phi)
  const N = A / Math.sqrt(1 - E2 * sin * sin)
  const T = tan * tan
  const C = EP2 * cos * cos
  const a = cos * (lam - lam0)
  const e4 = E2 * E2, e6 = e4 * E2
  const M = A * (
    (1 - E2 / 4 - (3 * e4) / 64 - (5 * e6) / 256) * phi
    - ((3 * E2) / 8 + (3 * e4) / 32 + (45 * e6) / 1024) * Math.sin(2 * phi)
    + ((15 * e4) / 256 + (45 * e6) / 1024) * Math.sin(4 * phi)
    - ((35 * e6) / 3072) * Math.sin(6 * phi)
  )
  const easting = K0 * N * (a + ((1 - T + C) * a ** 3) / 6 + ((5 - 18 * T + T * T + 72 * C - 58 * EP2) * a ** 5) / 120) + 500000
  let northing = K0 * (M + N * tan * ((a * a) / 2 + ((5 - T + 9 * C + 4 * C * C) * a ** 4) / 24 + ((61 - 58 * T + T * T + 600 * C - 330 * EP2) * a ** 6) / 720))
  if (lat < 0) northing += 10_000_000
  const band = BANDS[Math.max(0, Math.min(20, Math.floor((lat + 80) / 8)))]
  return { zone, band, easting, northing }
}

/** MGRS to `digits` per axis (5 = 1 m, 4 = 10 m), grouped "38T KM 12345 67890". */
export function toMgrs(p: LatLon, digits = 5): string {
  const u = toUtm(p)
  const set = u.zone % 6 || 6
  const col = COLS[(set - 1) % 3][Math.floor(u.easting / 100000) - 1] ?? '?'
  const row = ROWS[(Math.floor(u.northing / 100000) + (set % 2 === 0 ? 5 : 0)) % 20]
  const cut = (v: number) => pad(Math.floor(v % 100000), 5).slice(0, digits)
  return `${u.zone}${u.band} ${col}${row} ${cut(u.easting)} ${cut(u.northing)}`
}

export function fmtCoord(p: LatLon, f: CoordFormat): string {
  switch (f) {
    case 'ddm': return `${ddm(p.lat, 'N', 'S', 2)} ${ddm(p.lon, 'E', 'W', 3)}`
    case 'dms': return `${dms(p.lat, 'N', 'S', 2)} ${dms(p.lon, 'E', 'W', 3)}`
    case 'dec': return `${p.lat.toFixed(5)}, ${p.lon.toFixed(5)}`
    case 'mgrs': return toMgrs(p)
  }
}
