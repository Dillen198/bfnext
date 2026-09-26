/**
 * Range sectors: the parts of the theatre with one job each (a bombing
 * range, a fight area, a tanker track, the carrier's operating area...).
 * Same colours and names as the mission draws on the F10 map, so the site
 * and the game agree.
 */
import { circleRing, destination, fromLocal, rad, toLocal, type LatLon } from './geo'
import type { Sector, SectorKind, SectorShape, SectorTrack } from '../types'

export interface SectorKindInfo {
  /** as the F10 map labels it */
  label: string
  /** 0xRRGGBB as the F10 map draws it */
  color: string
  /** drawn with a dashed outline in game */
  dashed?: boolean
}

export const SECTOR_KINDS: Record<SectorKind, SectorKindInfo> = {
  air_to_ground: { label: 'AIR-TO-GROUND', color: '#FF9F1A' },
  tactical: { label: 'TACTICAL / CAS', color: '#FFE14D' },
  threat: { label: 'THREAT / SEAD', color: '#FF3B3B' },
  gunnery: { label: 'CA GUNNERY', color: '#C98A4B' },
  helo: { label: 'HELICOPTER', color: '#7CFC4A' },
  air_to_air: { label: 'AIR-TO-AIR', color: '#33D6FF' },
  bvr: { label: 'BVR', color: '#4D8DFF' },
  duel: { label: 'DUELS', color: '#FF66C4' },
  aar: { label: 'AIR REFUELLING', color: '#3DFF88', dashed: true },
  carrier: { label: 'CARRIER OPS', color: '#F2F2F2' },
  anti_ship: { label: 'ANTI-SHIP', color: '#C266FF' },
}

/** Legend order: ground work first, then the air, then the sea. */
export const SECTOR_KIND_ORDER: SectorKind[] = [
  'air_to_ground', 'tactical', 'threat', 'gunnery', 'helo',
  'air_to_air', 'bvr', 'duel', 'aar', 'carrier', 'anti_ship',
]

/** A kind this site does not know yet (a newer engine) still draws, in grey. */
export function kindInfo(kind: string): SectorKindInfo {
  return (SECTOR_KINDS as Record<string, SectorKindInfo | undefined>)[kind]
    ?? { label: kind.replace(/_/g, ' ').toUpperCase(), color: '#93A3B5' }
}

// ─── sides ─────────────────────────────────────────────────────────────────

export type SideFilter = 'all' | 'blue' | 'red'

/** "blue" / "red" / "all" (anything else counts as shared). */
export function sectorSide(s: Pick<Sector, 'side'>): SideFilter {
  const v = String(s.side ?? 'all').toLowerCase()
  return v === 'blue' || v === 'red' ? v : 'all'
}

/** Shared sectors show whichever side is picked. */
export function showsFor(s: Pick<Sector, 'side'>, filter: SideFilter): boolean {
  const side = sectorSide(s)
  return filter === 'all' || side === 'all' || side === filter
}

export function sideLabel(side: SideFilter): string {
  return side === 'blue' ? 'Blue' : side === 'red' ? 'Red' : 'Both sides'
}

// ─── geometry ──────────────────────────────────────────────────────────────

type Ring = [number, number][]

const finite = (...xs: number[]) => xs.every(Number.isFinite)

/**
 * The stadium around a tanker track: everything within `width_m / 2` of the
 * leg that starts at `lat`/`lon` and runs `leg_m` along `heading_deg`. A
 * closed ring in GeoJSON order ([lon, lat]), counter-clockwise.
 */
export function trackRing(t: SectorTrack, capSteps = 24): Ring {
  const o = { lat: t.lat, lon: t.lon }
  const r = t.width_m / 2
  const h = rad(t.heading_deg)
  const ch = Math.cos(h)
  const sh = Math.sin(h)
  // u along the leg, w to its right
  const at = (u: number, w: number): [number, number] => {
    const p = fromLocal(o, u * ch - w * sh, u * sh + w * ch)
    return [p.lon, p.lat]
  }
  const ring: Ring = []
  // round the far end from the right of the leg to the left...
  for (let i = 0; i <= capSteps; i++) {
    const a = (i / capSteps) * Math.PI
    ring.push(at(t.leg_m + r * Math.sin(a), r * Math.cos(a)))
  }
  // ...and back round the near end to where it started
  for (let i = 0; i <= capSteps; i++) {
    const a = (i / capSteps) * Math.PI
    ring.push(at(-r * Math.sin(a), -r * Math.cos(a)))
  }
  ring.push(ring[0])
  return ring
}

/** The sector's outline as a closed ring, or null when the shape is unusable. */
export function sectorRing(shape: SectorShape): Ring | null {
  const { polygon, circle, track } = shape
  if (polygon) {
    const pts = polygon.filter(p => finite(p.lat, p.lon))
    if (pts.length < 3) return null
    const ring: Ring = pts.map(p => [p.lon, p.lat])
    const [a, z] = [ring[0], ring[ring.length - 1]]
    if (a[0] !== z[0] || a[1] !== z[1]) ring.push([a[0], a[1]])
    return ring
  }
  if (circle) {
    if (!finite(circle.lat, circle.lon, circle.radius_m) || circle.radius_m <= 0) return null
    return circleRing(circle, circle.radius_m, 72)
  }
  if (track) {
    const { lat, lon, heading_deg, leg_m, width_m } = track
    if (!finite(lat, lon, heading_deg, leg_m, width_m) || leg_m <= 0 || width_m <= 0) return null
    return trackRing(track)
  }
  return null
}

/** How far up from the middle towards the northern edge a name sits. */
const LABEL_RISE = 0.7

/**
 * Where the sector's name goes: up from the middle towards the northern edge
 * of a circle or polygon (the middle is where the carrier or the targets
 * are), halfway down a tanker track's leg.
 */
export function sectorLabelPoint(shape: SectorShape, ring: Ring): LatLon {
  if (shape.circle) return destination(shape.circle, 0, shape.circle.radius_m * LABEL_RISE)
  if (shape.track) return destination(shape.track, shape.track.heading_deg, shape.track.leg_m / 2)
  const c = sectorCentre(ring)
  // the northern edge straight above the centre (east = 0 in c's frame)
  let top = 0
  const xy = ring.map(([lon, lat]) => toLocal(c, { lat, lon }))
  for (let i = 0; i < xy.length - 1; i++) {
    const p = xy[i], q = xy[i + 1]
    if ((p.east <= 0 && q.east >= 0) || (p.east >= 0 && q.east <= 0)) {
      const n = p.east === q.east ? Math.max(p.north, q.north) : p.north - (p.east * (q.north - p.north)) / (q.east - p.east)
      top = Math.max(top, n)
    }
  }
  return fromLocal(c, top * LABEL_RISE, 0)
}

/** The area centroid of a closed ring (the vertex mean if it has no area). */
export function sectorCentre(ring: Ring): LatLon {
  const o = { lat: ring[0][1], lon: ring[0][0] }
  const xy = ring.map(([lon, lat]) => toLocal(o, { lat, lon }))
  let a2 = 0, cx = 0, cy = 0
  for (let i = 0; i < xy.length - 1; i++) {
    const p = xy[i], q = xy[i + 1]
    const c = p.east * q.north - q.east * p.north
    a2 += c
    cx += (p.east + q.east) * c
    cy += (p.north + q.north) * c
  }
  if (Math.abs(a2) < 1) {
    const n = ring.length - 1
    return {
      lat: ring.slice(0, n).reduce((s, p) => s + p[1], 0) / n,
      lon: ring.slice(0, n).reduce((s, p) => s + p[0], 0) / n,
    }
  }
  return fromLocal(o, cy / (3 * a2), cx / (3 * a2))
}

/** Size of a closed ring in metres: east-west and north-south extent, and area. */
export function ringSize(ring: Ring): { width_m: number; height_m: number; area_m2: number } {
  const o = { lat: ring[0][1], lon: ring[0][0] }
  let w = Infinity, e = -Infinity, s = Infinity, n = -Infinity, a2 = 0
  let prev: { north: number; east: number } | null = null
  for (const [lon, lat] of ring) {
    const p = toLocal(o, { lat, lon })
    w = Math.min(w, p.east); e = Math.max(e, p.east)
    s = Math.min(s, p.north); n = Math.max(n, p.north)
    if (prev) a2 += prev.east * p.north - p.east * prev.north
    prev = p
  }
  return { width_m: e - w, height_m: n - s, area_m2: Math.abs(a2) / 2 }
}

/** A sector ready to draw. */
export interface DrawnSector {
  sector: Sector
  info: SectorKindInfo
  side: SideFilter
  ring: Ring
  label: LatLon
  /** east-west extent, metres: how much room its name has */
  width_m: number
  area_m2: number
}

/**
 * Every drawable sector, largest first, so smaller ones paint on top and win
 * when the pointer is over both.
 */
export function drawSectors(sectors: Sector[] | undefined): DrawnSector[] {
  const out: DrawnSector[] = []
  for (const s of sectors ?? []) {
    const ring = sectorRing(s.shape ?? {})
    if (!ring) continue
    const { width_m, area_m2 } = ringSize(ring)
    out.push({ sector: s, info: kindInfo(s.kind), side: sectorSide(s), ring, label: sectorLabelPoint(s.shape, ring), width_m, area_m2 })
  }
  return out.sort((a, b) => b.area_m2 - a.area_m2)
}

/** Ground metres per screen pixel at a maplibre zoom (512 px tiles). */
export function metresPerPixel(zoom: number, lat: number): number {
  return (40_075_016.686 * Math.cos(rad(lat))) / (512 * 2 ** zoom)
}
