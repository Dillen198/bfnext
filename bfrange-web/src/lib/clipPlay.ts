/**
 * Playing a debrief clip: where every object is at any time, in a local
 * frame of metres east / north / up around the action (a clip spans a few
 * tens of kilometres, so a flat frame is well inside a metre), and the
 * numbers the HUD reads off it.
 */
import { deckFor, landingFromRef, type DeckGeom } from './carrierDeck'
import { FT, KT, norm180 } from './geo'
import type { Clip, ClipObj } from '../types'

const M_PER_DEG = 111_320
const D2R = Math.PI / 180

export interface Frame {
  lat0: number
  lon0: number
  k: number
  /** lat/lon/alt → [east, north, up] metres */
  toLocal(lat: number, lon: number, alt: number): [number, number, number]
}

/** A frame centred on the pilot's first sample. */
export function frameFor(c: Clip): Frame {
  const me = c.objects.find(o => o.role === 'self') ?? c.objects[0]
  const lat0 = me?.lat[0] ?? 0
  const lon0 = me?.lon[0] ?? 0
  const k = Math.cos(lat0 * D2R)
  return { lat0, lon0, k, toLocal: (lat, lon, alt) => [(lon - lon0) * M_PER_DEG * k, (lat - lat0) * M_PER_DEG, alt] }
}

export interface State {
  x: number
  y: number
  z: number
  roll: number
  pitch: number
  /** true heading, degrees */
  yaw: number
  /** ground speed, knots, from the path */
  gs_kts: number
  /** climb, feet per minute */
  vs_fpm: number
  /** before its first sample or after it left */
  hidden: boolean
}

/** The last sample at or before `t` (binary search). */
function index(ts: number[], t: number): number {
  let lo = 0, hi = ts.length - 1
  if (t <= ts[0]) return 0
  if (t >= ts[hi]) return hi
  while (hi - lo > 1) {
    const m = (lo + hi) >> 1
    if (ts[m] <= t) lo = m
    else hi = m
  }
  return lo
}

const lerpAngle = (a: number, b: number, f: number) => a + norm180(b - a) * f

/** An object's interpolated state at clip time `t`. */
export function stateAt(o: ClipObj, f: Frame, t: number): State {
  const n = o.t.length
  const i = index(o.t, t)
  const j = Math.min(n - 1, i + 1)
  const span = o.t[j] - o.t[i]
  const u = span > 0 ? Math.max(0, Math.min(1, (t - o.t[i]) / span)) : 0
  const [x0, y0, z0] = f.toLocal(o.lat[i], o.lon[i], o.alt[i])
  const [x1, y1, z1] = f.toLocal(o.lat[j], o.lon[j], o.alt[j])
  const att = o.roll.length === n
  // speed from the bracketing samples (or the last two, at the end)
  const a = j > i ? i : Math.max(0, i - 1)
  const b = j > i ? j : i
  const [ax, ay, az] = f.toLocal(o.lat[a], o.lon[a], o.alt[a])
  const [bx, by, bz] = f.toLocal(o.lat[b], o.lon[b], o.alt[b])
  const dt = o.t[b] - o.t[a]
  const gs = dt > 0 ? Math.hypot(bx - ax, by - ay) / dt : 0
  const vs = dt > 0 ? (bz - az) / dt : 0
  const pathYaw = Math.hypot(bx - ax, by - ay) > 0.5 ? (Math.atan2(bx - ax, by - ay) / D2R + 360) % 360 : 0
  return {
    x: x0 + (x1 - x0) * u,
    y: y0 + (y1 - y0) * u,
    z: z0 + (z1 - z0) * u,
    roll: att ? o.roll[i] + (o.roll[j] - o.roll[i]) * u : 0,
    pitch: att ? o.pitch[i] + (o.pitch[j] - o.pitch[i]) * u : (dt > 0 && gs > 1 ? Math.atan2(vs, gs) / D2R : 0),
    yaw: att ? (lerpAngle(o.yaw[i], o.yaw[j], u) + 360) % 360 : pathYaw,
    gs_kts: gs / KT,
    vs_fpm: (vs / FT) * 60,
    hidden: t < o.t[0] - 0.01 || (o.gone !== undefined && t > o.gone + 0.01) || t > o.t[n - 1] + 0.5,
  }
}

/** The carrier pass numbers the LSO would read, from the pilot and the boat at `t`. */
export interface PassRead {
  /** metres aft of the landing point along the final bearing */
  x_m: number
  /** metres right of the landing-area centreline */
  y_m: number
  /** feet above the deck */
  hgt_ft: number
  /** glideslope error, degrees, + = high */
  gse_deg: number
  /** lineup error, degrees, + = right */
  lue_deg: number
  /** the landing point and final bearing in the local frame */
  lp: [number, number, number]
  fb_deg: number
}

export function boatGeom(o: ClipObj): DeckGeom {
  return deckFor(o.name)
}

export function passRead(me: State, boat: State, g: DeckGeom): PassRead {
  const h = boat.yaw * D2R
  const fwd: [number, number] = [Math.sin(h), Math.cos(h)]
  const right: [number, number] = [Math.cos(h), -Math.sin(h)]
  const l = landingFromRef(g)
  const lp: [number, number, number] = [
    boat.x + fwd[0] * l.fwd + right[0] * l.right,
    boat.y + fwd[1] * l.fwd + right[1] * l.right,
    boat.z + g.deck_height_m,
  ]
  const fb = boat.yaw + g.angle_deg
  const fr = fb * D2R
  const ufwd: [number, number] = [Math.sin(fr), Math.cos(fr)]
  const uright: [number, number] = [Math.cos(fr), -Math.sin(fr)]
  const dx = me.x - lp[0], dy = me.y - lp[1]
  const x = -(dx * ufwd[0] + dy * ufwd[1])
  const y = dx * uright[0] + dy * uright[1]
  const hgt = me.z - lp[2]
  const gse = x > 1 ? Math.atan2(hgt, x) / D2R - g.glideslope_deg : 0
  const lue = x > 1 ? Math.atan2(y, x) / D2R : 0
  return { x_m: x, y_m: y, hgt_ft: hgt / FT, gse_deg: gse, lue_deg: lue, lp, fb_deg: (fb + 360) % 360 }
}
