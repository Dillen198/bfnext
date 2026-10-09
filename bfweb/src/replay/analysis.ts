// Debrief maths for the replay: things DCS does not write into its Tacview
// recordings (G, Mach) worked out from the track, and the geometry between two
// objects (BRAA, closure, aspect) that a BVR debrief is built on.

import { type Seg, type State, bearingDeg, distM } from './data'

const G0 = 9.80665
const DEG = Math.PI / 180

/** Speed of sound (m/s) at an altitude (m), ISA. */
export function soundSpeed(altM: number): number {
  const h = Math.min(Math.max(altM, -500), 11_000)
  const tK = 288.15 - 0.0065 * h
  return Math.sqrt(1.4 * 287.05 * tK)
}

/** ISA density ratio, for an indicated-airspeed estimate. */
function densityRatio(altM: number): number {
  const h = Math.min(Math.max(altM, -500), 20_000)
  if (h <= 11_000) return Math.pow(1 - 2.25577e-5 * h, 4.2559)
  return 0.2971 * Math.exp(-(h - 11_000) / 6341.6)
}

/** Velocity (m/s, east/north/up) of a segment at sample i, central difference. */
function vel(s: Seg, i: number): [number, number, number] | null {
  const a = Math.max(0, i - 1), b = Math.min(s.t.length - 1, i + 1)
  const dt = (s.t[b] - s.t[a]) / 1000
  if (dt <= 0) return null
  const lat = (s.lat[a] + s.lat[b]) / 2
  return [
    ((s.lon[b] - s.lon[a]) * 111_320 * Math.cos(lat * DEG)) / dt,
    ((s.lat[b] - s.lat[a]) * 110_540) / dt,
    (s.alt[b] - s.alt[a]) / dt,
  ]
}

/** Load factor (G) at sample i: |acceleration + gravity| / g. Smoothed over
 *  ~2 s, because the track is sampled twice a second. */
export function gAt(s: Seg, i: number): number | null {
  const n = s.t.length
  if (n < 5) return null
  const a = Math.max(1, i - 2), b = Math.min(n - 2, i + 2)
  if (b <= a) return null
  const va = vel(s, a), vb = vel(s, b)
  if (!va || !vb) return null
  const dt = (s.t[b] - s.t[a]) / 1000
  if (dt <= 0) return null
  const ax = (vb[0] - va[0]) / dt, ay = (vb[1] - va[1]) / dt, az = (vb[2] - va[2]) / dt + G0
  return Math.sqrt(ax * ax + ay * ay + az * az) / G0
}

/** Index of the sample nearest t. */
export function nearest(s: Seg, t: number): number {
  let lo = 0, hi = s.t.length - 1
  while (lo < hi) {
    const mid = (lo + hi) >> 1
    if (s.t[mid] < t) lo = mid + 1
    else hi = mid
  }
  if (lo > 0 && Math.abs(s.t[lo - 1] - t) < Math.abs(s.t[lo] - t)) return lo - 1
  return lo
}

/** True airspeed is not in DCS's recordings; ground speed plus climb is the
 *  best stand-in (no wind data). */
export function tasOf(st: State): number {
  return Math.sqrt(st.gs * st.gs + st.vs * st.vs)
}
export function machOf(st: State): number {
  return st.mach ?? tasOf(st) / soundSpeed(st.alt)
}
/** Indicated airspeed: the recorded one, else estimated from TAS and ISA density. */
export function iasOf(st: State): number {
  return st.ias ?? tasOf(st) * Math.sqrt(densityRatio(st.alt))
}

/** Everything a flight did, from its whole track. */
export interface FlightStats {
  maxAlt: number
  maxSpeed: number
  maxMach: number
  maxG: number
  minG: number
  distanceM: number
}
export function flightStats(s: Seg): FlightStats {
  const st: FlightStats = { maxAlt: 0, maxSpeed: 0, maxMach: 0, maxG: 1, minG: 1, distanceM: 0 }
  for (let i = 0; i < s.t.length; i++) {
    st.maxAlt = Math.max(st.maxAlt, s.alt[i])
    if (i > 0) {
      const d = distM(s.lon[i - 1], s.lat[i - 1], s.lon[i], s.lat[i])
      st.distanceM += d
      const dt = (s.t[i] - s.t[i - 1]) / 1000
      if (dt > 0) {
        const v = Math.hypot(d, s.alt[i] - s.alt[i - 1]) / dt
        // a dropped frame shows up as a teleport; ignore the impossible
        if (v < 1200) {
          st.maxSpeed = Math.max(st.maxSpeed, v)
          st.maxMach = Math.max(st.maxMach, v / soundSpeed(s.alt[i]))
        }
      }
    }
    if (i % 2 === 0) {
      const g = gAt(s, i)
      if (g != null && g < 15) {
        st.maxG = Math.max(st.maxG, g)
        st.minG = Math.min(st.minG, g)
      }
    }
  }
  return st
}

/** The picture of `b` as seen from `a`. */
export interface Braa {
  /** slant range, m */
  range: number
  /** true bearing a -> b, deg */
  bearing: number
  /** b's altitude minus a's, m */
  dAlt: number
  /** closing speed, m/s (positive = closing) */
  closure: number
  /** aspect angle: angle off b's tail as seen from a, 0 = a is dead astern of b, 180 = head on */
  aspect: number
  /** antenna train angle: b's angle off a's nose, deg */
  ata: number
}

function velOfState(st: State): [number, number, number] {
  return [Math.sin(st.hdg * DEG) * st.gs, Math.cos(st.hdg * DEG) * st.gs, st.vs]
}

export function braa(a: State, b: State): Braa {
  const brg = bearingDeg(a.lon, a.lat, b.lon, b.lat)
  const ground = distM(a.lon, a.lat, b.lon, b.lat)
  const dAlt = b.alt - a.alt
  const range = Math.hypot(ground, dAlt)
  // line of sight a -> b, east/north/up unit vector
  const los: [number, number, number] = [
    (Math.sin(brg * DEG) * ground) / (range || 1),
    (Math.cos(brg * DEG) * ground) / (range || 1),
    dAlt / (range || 1),
  ]
  const va = velOfState(a), vb = velOfState(b)
  const rel = [vb[0] - va[0], vb[1] - va[1], vb[2] - va[2]]
  const closure = -(rel[0] * los[0] + rel[1] * los[1] + rel[2] * los[2])
  // aspect: angle between b's heading and the line b -> a, measured off b's tail
  const recip = (brg + 180) % 360
  let off = Math.abs(((recip - b.hdg + 540) % 360) - 180) // 0 = a is on b's nose
  off = 180 - off // 0 = a is astern of b
  const ata = Math.abs(((brg - a.hdg + 540) % 360) - 180)
  return { range, bearing: brg, dAlt, closure, aspect: off, ata }
}

/** Cardinal-ish aspect words, as a controller would say them. */
export function aspectWord(aspect: number): string {
  if (aspect >= 150) return 'HOT'
  if (aspect >= 110) return 'FLANK'
  if (aspect >= 70) return 'BEAM'
  if (aspect >= 30) return 'DRAG'
  return 'COLD'
}

export const M_TO_NM = 1 / 1852
