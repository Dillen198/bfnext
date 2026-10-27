// Route maths for the Planner: legs, times, and the step from lat/lon to the
// DCS map metres a native cartridge stores.

import geomagnetism from 'geomagnetism'
import proj4 from 'proj4'
import { getRange, toMercProj } from '../scope/util'
import { Terrains, type Terrain } from '../scope/dcs/terrain'
import type { Plan } from './types'

const toRad = (d: number) => (d * Math.PI) / 180
const toDeg = (r: number) => (r * 180) / Math.PI

/** Great-circle distance, nautical miles. */
export function distanceNm(a: [number, number], b: [number, number]): number {
  return getRange(a, b)
}

/** Initial great-circle course from `a` to `b`, degrees true, 0-360. */
export function courseTrue([lat1, lon1]: [number, number], [lat2, lon2]: [number, number]): number {
  const p1 = toRad(lat1)
  const p2 = toRad(lat2)
  const dl = toRad(lon2 - lon1)
  const y = Math.sin(dl) * Math.cos(p2)
  const x = Math.cos(p1) * Math.sin(p2) - Math.sin(p1) * Math.cos(p2) * Math.cos(dl)
  return (toDeg(Math.atan2(y, x)) + 360) % 360
}

/** Magnetic variation at a point, degrees, east positive. The same WMM epoch
 *  the radar scope uses, so the two pages agree on a magnetic heading. */
export type MagVar = (lat: number, lon: number) => number

let model: { point(ll: [number, number]): { decl: number } } | null = null
export const wmmMagVar: MagVar = (lat, lon) => {
  if (!model) {
    try { model = geomagnetism.model(new Date(2024, 5, 1)) }
    catch { model = { point: () => ({ decl: 0 }) } }
  }
  return model.point([lat, lon]).decl
}

export interface Leg {
  /** Index of the waypoint this leg ends at. */
  to: number
  dist_nm: number
  course_true: number
  course_mag: number
  /** Leg time at the plan's speed, seconds. */
  ete_s: number
  /** Time from the first waypoint to the end of this leg, seconds. */
  elapsed_s: number
}

export function legs(plan: Plan, magVar: MagVar = wmmMagVar): Leg[] {
  const out: Leg[] = []
  let elapsed = 0
  const w = plan.waypoints
  for (let i = 1; i < w.length; i++) {
    const a: [number, number] = [w[i - 1].lat, w[i - 1].lon]
    const b: [number, number] = [w[i].lat, w[i].lon]
    const dist = distanceNm(a, b)
    const crs = courseTrue(a, b)
    const ete = plan.speed_kts > 0 ? (dist / plan.speed_kts) * 3600 : 0
    elapsed += ete
    out.push({
      to: i,
      dist_nm: dist,
      course_true: crs,
      course_mag: (crs - magVar(a[0], a[1]) + 360) % 360,
      ete_s: ete,
      elapsed_s: elapsed,
    })
  }
  return out
}

/** "HH:MM[:SS]" to seconds since midnight, or null. */
export function parseClock(t: string | null | undefined): number | null {
  const m = /^(\d{2}):(\d{2})(?::(\d{2}))?$/.exec((t ?? '').trim())
  if (!m) return null
  const [h, mi, s] = [+m[1], +m[2], +(m[3] ?? 0)]
  if (h > 23 || mi > 59 || s > 59) return null
  return h * 3600 + mi * 60 + s
}

/** Seconds to "HH:MM:SS", wrapping past midnight. */
export function fmtClock(sec: number): string {
  const s = ((Math.round(sec) % 86400) + 86400) % 86400
  const p = (n: number) => String(n).padStart(2, '0')
  return `${p(Math.floor(s / 3600))}:${p(Math.floor((s % 3600) / 60))}:${p(s % 60)}`
}

/** Seconds to "M:SS" or "H:MM:SS" for a leg time. */
export function fmtDuration(sec: number): string {
  const s = Math.round(sec)
  const h = Math.floor(s / 3600)
  const m = Math.floor((s % 3600) / 60)
  const ss = String(s % 60).padStart(2, '0')
  return h > 0 ? `${h}:${String(m).padStart(2, '0')}:${ss}` : `${m}:${ss}`
}

/** The DCS terrain id (what the mission editor's `getCurrentTerrainID`
 *  returns) for each map the scope knows. A native cartridge carrying the
 *  wrong one has its nav points silently dropped on load. */
const DCS_TERRAIN_ID: Record<string, string> = {
  Caucasus: 'Caucasus',
  Falklands: 'Falklands',
  Germany: 'GermanyCW',
  Kola: 'Kola',
  MarianaIslands: 'MarianaIslands',
  Nevada: 'Nevada',
  Normandy: 'Normandy',
  PersianGulf: 'PersianGulf',
  Sinai: 'SinaiMap',
  Syria: 'Syria',
  TheChannel: 'TheChannel',
}

export interface DcsTerrain {
  terrain: Terrain
  /** The id a native cartridge carries. */
  id: string
}

/** Every DCS map the planner knows, by name. */
export const DCS_MAPS: string[] = Terrains.map(t => t.name)

/** A DCS map by name ("Syria"). */
export function terrainNamed(name: string): DcsTerrain | null {
  const t = Terrains.find(x => x.name === name)
  return t ? { terrain: t, id: DCS_TERRAIN_ID[t.name] ?? t.name } : null
}

/** The DCS map a point is on: the nearest map centre within 500 nm. */
export function terrainAt(lat: number, lon: number): DcsTerrain | null {
  let best: Terrain | null = null
  let bestD = Infinity
  for (const t of Terrains) {
    const d = getRange([lat, lon], t.center)
    if (d < bestD) { best = t; bestD = d }
  }
  if (!best || bestD > 500) return null
  return { terrain: best, id: DCS_TERRAIN_ID[best.name] ?? best.name }
}

/** Lat/lon to DCS map metres: x is north, y is east. proj4 hands back
 *  [easting, northing] whatever the axis flag says, hence the swap. */
export function toDcsXY(lat: number, lon: number, terrain: Terrain): { x: number; y: number } {
  const [east, north] = toMercProj([lat, lon], terrain)
  return { x: north, y: east }
}

/** DCS map metres back to lat/lon: the inverse of `toDcsXY`, same
 *  transverse Mercator (pydcs' parameters, as the scope uses). */
export function fromDcsXY(x: number, y: number, terrain: Terrain): { lat: number; lon: number } {
  const p = terrain.projection
  const def = [
    '+proj=tmerc', '+lat_0=0', `+lon_0=${p.centralMeridian}`, `+k_0=${p.scaleFactor}`,
    `+x_0=${p.falseEasting}`, `+y_0=${p.falseNorthing}`, '+towgs84=0,0,0,0,0,0,0', '+units=m', '+ellps=WGS84', '+no_defs',
  ].join(' ')
  const [lon, lat] = proj4(def).inverse([y, x])
  return { lat, lon }
}

/**
 * A Hornet route split into its three sequences of at most 15 points. Each
 * sequence after the first starts at the previous one's last point, so the
 * leg between them is flown in a sequence too: 1-15, 15-29, 29-43. Indices
 * into the route; points past 43 are in no sequence.
 */
export function hornetSequences(n: number): number[][] {
  const out: number[][] = []
  let start = 0
  while (out.length < 3 && start < n) {
    const end = Math.min(start + 15, n)
    out.push(Array.from({ length: end - start }, (_, k) => start + k))
    if (end >= n) break
    start = end - 1
  }
  return out
}

/** The point `rangeM` metres from (lat, lon) along a true bearing: great
 *  circle on a sphere, plenty for offsets of a few miles. */
export function destination(lat: number, lon: number, bearingTrueDeg: number, rangeM: number): { lat: number; lon: number } {
  const R = 6371008.8
  const d = rangeM / R
  const b = (bearingTrueDeg * Math.PI) / 180
  const p1 = (lat * Math.PI) / 180
  const l1 = (lon * Math.PI) / 180
  const p2 = Math.asin(Math.sin(p1) * Math.cos(d) + Math.cos(p1) * Math.sin(d) * Math.cos(b))
  const l2 = l1 + Math.atan2(Math.sin(b) * Math.sin(d) * Math.cos(p1), Math.cos(d) - Math.sin(p1) * Math.sin(p2))
  return { lat: (p2 * 180) / Math.PI, lon: ((((l2 * 180) / Math.PI) + 540) % 360) - 180 }
}

/** The grid angle of true north at a point, radians from +x (grid north)
 *  toward +y: the meridian convergence of the map's projection. */
export function gridConvergence(lat: number, lon: number, terrain: Terrain): number {
  const a = toDcsXY(lat, lon, terrain)
  const n = toDcsXY(lat + 0.01, lon, terrain)
  return Math.atan2(n.y - a.y, n.x - a.x)
}

/**
 * An offset from a waypoint as a map position, with its grid geometry from
 * the anchor: what DCS's cartridges store. DCS's world is the flat map grid,
 * so the range is laid out in grid metres (the cockpit's 3000 ft is 914.4 m
 * of x/y); only the bearing is turned from true to grid.
 */
export function offsetOnGrid(anchorLat: number, anchorLon: number, off: { range_ft: number; bearing_deg: number }, terrain: Terrain) {
  const a = toDcsXY(anchorLat, anchorLon, terrain)
  const r = off.range_ft * 0.3048
  const b = (((off.bearing_deg * Math.PI) / 180 + gridConvergence(anchorLat, anchorLon, terrain)) % (2 * Math.PI) + 2 * Math.PI) % (2 * Math.PI)
  const x = a.x + r * Math.cos(b)
  const y = a.y + r * Math.sin(b)
  const ll = fromDcsXY(x, y, terrain)
  return { x, y, lat: ll.lat, lon: ll.lon, range: r, bearingRad: b, deltaX: a.x - x, deltaY: a.y - y }
}
