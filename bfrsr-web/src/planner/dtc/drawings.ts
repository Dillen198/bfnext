// What the jets' own DTC editors draw around a cartridge item, ported so the
// map shows what DCS shows: F-16 threat rings, Hornet MEZ rings, CAP orbits
// and corridor edges, F-14 JDAM launch zones with their launch point and
// heading handle. All built on the DCS map grid (x north, y east) as the
// editors do, then turned into lat/lon.

import { getAt, type Json, type JsonObject, type Seg } from '../model'
import { fromDcsXY, toDcsXY, type DcsTerrain } from '../nav'
import { lar, larPolygon, launchPoint } from '../f14lar'
import { isObj, type Ctx } from './core'

type XY = { x: number; y: number }

export interface DrawLine {
  coords: [number, number][]
  color: string
  dashed: boolean
  here: boolean
}
export interface DrawLabel { lat: number; lon: number; text: string; color: string; here: boolean }
/** A draggable point that edits its item: a JDAM launch point or heading. */
export interface Handle {
  key: string
  kind: 'launch' | 'heading'
  /** The item it edits. */
  segs: Seg[]
  lat: number
  lon: number
  label: string
  color: string
  here: boolean
}
export interface Drawings { lines: DrawLine[]; labels: DrawLabel[]; handles: Handle[] }

/** Colours as the editors use them. */
export const COLOURS = { f16Ring: '#facc15', mez: '#f87171', cap: '#60a5fa', corridor: '#a3e635', lar: '#ff6464' }
const CORRIDOR_HALF_WIDTH = 5 * 1852

const under = (segs: Seg[], root: Seg[]) => root.length > 0 && root.every((s, i) => String(segs[i]) === String(s))

export function drawings(ctx: Ctx, map: DcsTerrain, pageSegs: Seg[]): Drawings {
  const out: Drawings = { lines: [], labels: [], handles: [] }
  const ll = (p: XY): [number, number] => { const q = fromDcsXY(p.x, p.y, map.terrain); return [q.lon, q.lat] }
  const line = (pts: XY[], color: string, dashed: boolean, here: boolean) => out.lines.push({ coords: pts.map(ll), color, dashed, here })
  const label = (p: XY, text: string, color: string, here: boolean) => { const [lon, lat] = ll(p); out.labels.push({ lat, lon, text, color, here }) }
  const circle = (c: XY, r: number, n = 64): XY[] => Array.from({ length: n + 1 }, (_, i) => {
    const a = (i / n) * Math.PI * 2
    return { x: c.x + r * Math.cos(a), y: c.y + r * Math.sin(a) }
  })

  const walk = (v: Json | undefined, segs: Seg[], depth: number) => {
    if (depth > 9 || v === null || typeof v !== 'object') return
    if (ctx.idx.get(segs)?.hidden) return
    const here = under(segs, pageSegs)
    if (Array.isArray(v)) {
      if (segs[segs.length - 1] === 'CORRIDORS') v.forEach((c, i) => corridor(c, here, i))
      v.forEach((x, i) => walk(x, [...segs, i], depth + 1))
      return
    }
    const o = v as JsonObject
    const at = typeof o.x === 'number' && typeof o.y === 'number' && (o.x !== 0 || o.y !== 0) ? { x: o.x, y: o.y } : null
    if (at) {
      // F-16 pre-planned threat: its WEZ ring on the HSD.
      if ('def_num' in o && typeof o.radius === 'number' && o.radius > 0 && o.ring !== false) {
        line(circle(at, o.radius), COLOURS.f16Ring, false, here)
      }
      // Hornet MEZ: ring in nautical miles.
      if (typeof o.threat_ring_radius === 'number' && o.threat_ring_radius > 0) {
        line(circle(at, o.threat_ring_radius * 1852), COLOURS.mez, false, here)
        label(at, `${o.num ?? ''} ${o.text ?? ''}`.trim(), COLOURS.mez, here)
      }
      // Hornet CAP: the racetrack.
      if (typeof o.course === 'number' && typeof o.length === 'number' && typeof o.diameter === 'number') {
        line(capOrbit(at, o.course, o.diameter, o.length, o.turn_direction === 'Right' ? 'Right' : 'Left'), COLOURS.cap, false, here)
        label(at, `CAP ${o.num ?? ''}`.trim(), COLOURS.cap, here)
      }
      // F-14 JDAM target: launch zone, launch point, heading handle.
      if (o.active === true && typeof o.drop_alt === 'number' && typeof o.drop_spd === 'number') jdam(o, at, segs, here)
    }
    for (const [k, x] of Object.entries(o)) walk(x, [...segs, k], depth + 1)
  }

  const corridor = (c: Json, here: boolean, i: number) => {
    if (!isObj(c) || !Array.isArray(c.points)) return
    const pts = (c.points as JsonObject[]).filter(p => typeof p.x === 'number' && typeof p.y === 'number').map(p => ({ x: p.x as number, y: p.y as number }))
    if (pts.length < 2) return
    const [left, right] = corridorEdges(pts, CORRIDOR_HALF_WIDTH)
    line(left, COLOURS.corridor, true, here)
    line(right, COLOURS.corridor, true, here)
    label(pts[0], `CORR${c.num ?? i + 1}`, COLOURS.corridor, here)
  }

  const jdam = (t: JsonObject, at: XY, segs: Seg[], here: boolean) => {
    const head = typeof t.attack_heading === 'number' ? t.attack_heading : 0
    const l = lar(t.drop_alt as number, t.drop_spd as number)
    const poly = larPolygon(at.x, at.y, head, l)
    line([...poly, poly[0]], COLOURS.lar, false, here)
    const lp = launchPoint(at.x, at.y, head, l, { x: t.launch_x, y: t.launch_y })
    const back = (((head + 180) % 360) * Math.PI) / 180
    const far = { x: at.x + l.lar_rmax_nmi * 1852 * Math.cos(back), y: at.y + l.lar_rmax_nmi * 1852 * Math.sin(back) }
    // The run in: from the far end of the zone, through the launch point, to the target.
    line([far, lp, at], COLOURS.lar, true, here)
    const name = jdamName(segs)
    const [lon1, lat1] = ll(lp)
    const [lon2, lat2] = ll(far)
    const key = segs.join('/')
    out.handles.push({ key: `${key}/lp`, kind: 'launch', segs, lat: lat1, lon: lon1, label: `${name} LP`, color: COLOURS.lar, here })
    out.handles.push({ key: `${key}/hdg`, kind: 'heading', segs, lat: lat2, lon: lon2, label: `${Math.round(head)}°`, color: COLOURS.lar, here })
  }

  walk(ctx.value, [], 0)
  return out
}

/** "STA3 T1" for JDAM/stations/0/targets/0, as the ME labels it. */
function jdamName(segs: Seg[]): string {
  const s = segs.indexOf('stations')
  const t = segs.indexOf('targets')
  if (s < 0 || t < 0) return 'JDAM'
  return `STA${(segs[s + 1] as number) + 3} T${(segs[t + 1] as number) + 1}`
}

/** The CAP racetrack (port of drawCAP_Orbit): two legs `diameter` apart on
 *  the turn side, from the main point back along the course, joined by
 *  half circles at each end. */
export function capOrbit(main: XY, courseDeg: number, width: number, length: number, turn: 'Left' | 'Right'): XY[] {
  const c = (courseDeg * Math.PI) / 180
  const perp = c + Math.PI / 2
  const side = turn === 'Left' ? -1 : 1
  const off = (p: XY, d: number, a: number) => ({ x: p.x + d * Math.cos(a), y: p.y + d * Math.sin(a) })
  const p2 = main
  const p1 = off(main, side * width, perp)
  const p4 = off(main, -length, c)
  const p3 = off(p4, side * width, perp)
  // A half circle from a to b, bulging along `dir` (radians).
  const arc = (a: XY, b: XY, dir: number): XY[] => {
    const m = { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 }
    const r = Math.hypot(b.x - a.x, b.y - a.y) / 2
    const start = Math.atan2(a.y - m.y, a.x - m.x)
    // Sweep the way that passes through m + r*dir.
    const mid = Math.atan2(Math.sin(dir), Math.cos(dir))
    const ccw = Math.sin(mid - start) > 0 ? 1 : -1
    return Array.from({ length: 17 }, (_, i) => {
      const a2 = start + ccw * Math.PI * (i / 16)
      return { x: m.x + r * Math.cos(a2), y: m.y + r * Math.sin(a2) }
    })
  }
  // Out along the course leg, round the far end, back down the other leg, round the near end.
  return [p4, p2, ...arc(p2, p1, c).slice(1), p3, ...arc(p3, p4, c + Math.PI).slice(1)]
}

/** A corridor's two edges, `half` either side with mitred corners (port of
 *  computeBoundaryVertices). */
export function corridorEdges(pts: XY[], half: number): [XY[], XY[]] {
  const n = pts.length
  const normL: XY[] = []
  for (let i = 0; i < n - 1; i++) {
    const dx = pts[i + 1].x - pts[i].x
    const dy = pts[i + 1].y - pts[i].y
    const d = Math.hypot(dx, dy) || 1
    normL.push({ x: -dy / d, y: dx / d })
  }
  const miter = (p: XY, a: XY, b: XY, s: number): XY => {
    let bx = a.x + b.x
    let by = a.y + b.y
    const bl = Math.hypot(bx, by)
    if (bl < 0.001) return { x: p.x + s * b.x * half, y: p.y + s * b.y * half }
    bx /= bl
    by /= bl
    const cos = bx * a.x + by * a.y
    const k = Math.abs(cos) > 0.1 ? half / cos : half * 4
    return { x: p.x + s * bx * k, y: p.y + s * by * k }
  }
  const edge = (s: number) => pts.map((p, i) =>
    i === 0 ? { x: p.x + s * normL[0].x * half, y: p.y + s * normL[0].y * half }
      : i === n - 1 ? { x: p.x + s * normL[n - 2].x * half, y: p.y + s * normL[n - 2].y * half }
        : miter(p, normL[i - 1], normL[i], s))
  return [edge(1), edge(-1)]
}

/** The target after dragging one of its handles: the launch point (kept in
 *  the zone) or the attack heading (from the handle towards the target). */
export function moveHandle(ctx: Ctx, h: Handle, lat: number, lon: number, map: DcsTerrain): JsonObject {
  const t = { ...(getAt(ctx.value, h.segs) as JsonObject) }
  const p = toDcsXY(lat, lon, map.terrain)
  const at = { x: t.x as number, y: t.y as number }
  if (h.kind === 'heading') {
    const deg = (Math.atan2(at.y - p.y, at.x - p.x) * 180) / Math.PI
    t.attack_heading = Math.round((deg + 360) % 360) % 360
  } else {
    t.launch_x = p.x
    t.launch_y = p.y
  }
  // Keep the launch point in the (possibly turned) zone, as the ME does.
  if (typeof t.launch_x === 'number') {
    const l = lar(t.drop_alt as number, t.drop_spd as number)
    const lp = launchPoint(at.x, at.y, t.attack_heading as number, l, { x: t.launch_x, y: t.launch_y })
    t.launch_x = lp.x
    t.launch_y = lp.y
  }
  return t
}
