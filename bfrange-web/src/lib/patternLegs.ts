/**
 * The traffic pattern and the taxi of a field landing, in the runway frame:
 * `along` metres from the landing threshold (+ in the landing direction),
 * `across` metres (+ right of the centreline), heights AGL.
 *
 * A port of `bfprotocols/src/range/pattern_legs.rs` (which writes the
 * record's `pattern` / `taxi` summaries), so the debrief can colour every
 * leg of the track the way the engine read it, and the mock can make the
 * same summaries. Change one, change both.
 */
import { toLocal, type LatLon } from './geo'
import type { FieldLandingResult, FieldPattern, FieldTaxi, TrackPt } from '../types'

const NM = 1852
const M_TO_FT = 3.28084

export type Leg = 'initial' | 'break' | 'downwind' | 'base' | 'final' | 'rollout' | 'taxi' | 'other'

export interface RelPt {
  t: number
  along: number
  across: number
  /** metres above the field */
  agl: number
  /** track minus the landing direction, -180..180 */
  rel: number
  gs_kts: number
}

const wrap = (a: number) => {
  const x = ((((a + 180) % 360) + 360) % 360) - 180
  return x === -180 ? 180 : x
}

/** The runway frame of a landing: threshold, heading, field elevation. */
export function runwayFrame(r: FieldLandingResult) {
  const thr: LatLon = r.threshold_pos && (r.threshold_pos.lat || r.threshold_pos.lon) ? r.threshold_pos : r.touchdown_pos
  const hdg = r.runway_heading_deg ?? 0
  const h = (hdg * Math.PI) / 180
  const elev = r.threshold_pos?.alt_m ?? r.touchdown_pos.alt_m
  const toRwy = (p: LatLon) => {
    const { north, east } = toLocal(thr, p)
    return { along: north * Math.cos(h) + east * Math.sin(h), across: -north * Math.sin(h) + east * Math.cos(h) }
  }
  return { thr, hdg, elev, toRwy }
}

/** Track points in the runway frame, with the track direction worked out from the path. */
export function relPoints(pts: TrackPt[], r: FieldLandingResult): RelPt[] {
  const f = runwayFrame(r)
  const loc = pts.map(p => ({ p, ...f.toRwy(p) }))
  return loc.map((q, i) => {
    const a = loc[Math.max(0, i - 1)], b = loc[Math.min(loc.length - 1, i + 1)]
    const da = b.along - a.along, dc = b.across - a.across
    const rel = Math.hypot(da, dc) > 1 ? (Math.atan2(dc, da) * 180) / Math.PI : (i > 0 ? 0 : 0)
    return { t: q.p.t, along: q.along, across: q.across, agl: q.p.alt_m - f.elev, rel: wrap(rel), gs_kts: q.p.speed_kts }
  })
}

/**
 * The leg of every sample, walking back from the touchdown the way the
 * engine does: final, base, downwind, break, initial.
 */
export function legs(s: RelPt[], tTouch = 0): Leg[] {
  const out: Leg[] = s.map(x => (x.t > tTouch ? 'rollout' : 'other'))
  const pre = s.filter(x => x.t <= tTouch).length
  if (pre < 5) return out
  const aligned = (x: RelPt) => Math.abs(wrap(x.rel)) < 25
  const reciprocal = (x: RelPt) => 180 - Math.abs(wrap(x.rel)) < 25
  let i = pre - 1
  while (i > 0 && aligned(s[i - 1]) && Math.abs(s[i - 1].across) < 1500 && s[i - 1].along <= s[i].along + 50) i--
  for (let n = i; n < pre; n++) out[n] = 'final'
  let j = i
  while (j > 0 && !reciprocal(s[j - 1]) && !aligned(s[j - 1])) j--
  for (let n = j; n < i; n++) out[n] = 'base'
  let k = j
  while (k > 0 && reciprocal(s[k - 1])) k--
  for (let n = k; n < j; n++) out[n] = 'downwind'
  if (j - k >= 3) {
    let b = k
    while (b > 0 && !aligned(s[b - 1])) b--
    for (let n = b; n < k; n++) out[n] = 'break'
    let a = b
    while (a > 0 && aligned(s[a - 1]) && Math.abs(s[a - 1].across) < 900) a--
    for (let n = a; n < b; n++) out[n] = 'initial'
  }
  return out
}

const median = (v: number[]) => {
  if (!v.length) return null
  const s = [...v].sort((a, b) => a - b)
  return s[Math.floor(s.length / 2)]
}

/** The engine's pattern summary (pattern_legs.rs `pattern`). */
export function patternSummary(s: RelPt[], tTouch = 0): FieldPattern {
  const lg = legs(s, tTouch)
  const pick = (l: Leg) => s.filter((_, i) => lg[i] === l)
  const fin = pick('final'), dw = pick('downwind'), ini = pick('initial'), brk = pick('break'), base = pick('base')
  const p: FieldPattern = { kind: 'other', calls: [] }
  const roll = fin[0]
  if (roll) {
    p.rollout_nm = Math.max(0, -roll.along) / NM
    p.rollout_alt_ft = roll.agl * M_TO_FT
    p.final_gs_kts = median(fin.filter(x => x.along < 0).map(x => x.gs_kts)) ?? undefined
  }
  const dwSecs = dw.length ? dw[dw.length - 1].t - dw[0].t : 0
  if (dw.length >= 3 && dwSecs >= 8) {
    p.base_at_m = (base[0] ?? roll)?.along
    p.downwind_alt_ft = median(dw.map(x => x.agl * M_TO_FT)) ?? undefined
    const ab = dw.reduce((m, x) => (Math.abs(x.along) < Math.abs(m.along) ? x : m), dw[0])
    if (Math.abs(ab.along) < 1500) {
      p.abeam_nm = Math.abs(ab.across) / NM
      p.abeam_alt_ft = ab.agl * M_TO_FT
    }
    p.side = ab.across < 0 ? 'left' : 'right'
    const iniSecs = ini.length ? ini[ini.length - 1].t - ini[0].t : 0
    if (ini.length >= 2 && iniSecs >= 4) {
      const bp = ini[ini.length - 1]
      p.kind = 'overhead'
      p.break_at_m = bp.along
      p.break_alt_ft = bp.agl * M_TO_FT
      p.initial_alt_ft = median(ini.map(x => x.agl * M_TO_FT)) ?? undefined
      p.pattern_time_s = tTouch - bp.t
    } else {
      p.kind = 'downwind'
      p.pattern_time_s = tTouch - dw[0].t
    }
    void brk
  } else if (roll && Math.abs(fin[fin.length - 1].along - roll.along) > 1.5 * NM) {
    p.kind = 'straight_in'
    p.pattern_time_s = tTouch - roll.t
  } else if (roll) {
    p.pattern_time_s = tTouch - roll.t
  }
  if (p.abeam_nm != null) {
    if (p.abeam_nm > 1.8) p.calls.push(`wide downwind, ${p.abeam_nm.toFixed(1)} nm abeam`)
    else if (p.abeam_nm < 0.6) p.calls.push(`tight downwind, ${p.abeam_nm.toFixed(1)} nm abeam`)
  }
  if (p.downwind_alt_ft != null) {
    if (p.downwind_alt_ft < 600) p.calls.push(`low downwind, ${Math.round(p.downwind_alt_ft)} ft`)
    else if (p.downwind_alt_ft > 2500) p.calls.push(`high downwind, ${Math.round(p.downwind_alt_ft)} ft`)
  }
  if (p.base_at_m != null && p.base_at_m < -1.5 * NM) p.calls.push(`extended downwind, base ${(-p.base_at_m / NM).toFixed(1)} nm past the threshold`)
  if (p.kind !== 'straight_in' && p.rollout_nm != null && p.rollout_nm < 0.3) p.calls.push(`short final, rolled out ${p.rollout_nm.toFixed(2)} nm`)
  return p
}

export const TAXI_LIMIT_KTS = 30

/** The engine's taxi summary (pattern_legs.rs `taxi`); `s` from the touchdown on. */
export function taxiSummary(s: { t: number; along: number; across: number; gs_kts: number; x: number; y: number }[], rwyLen: number, rwyWidth: number, end: FieldTaxi['end']): FieldTaxi {
  const t: FieldTaxi = { rollout_m: 0, rollout_s: 0, taxi_time_s: 0, taxi_distance_m: 0, max_taxi_kts: 0, stops: 0, end, calls: [] }
  if (!s.length) return t
  const td = s[0]
  let slow = s.findIndex(x => x.gs_kts <= TAXI_LIMIT_KTS)
  if (slow < 0) slow = s.length - 1
  t.rollout_m = Math.max(0, s[slow].along - td.along)
  t.rollout_s = s[slow].t - td.t
  const onRwy = (x: (typeof s)[number]) => Math.abs(x.across) <= rwyWidth / 2 + 4 && x.along >= -30 && x.along <= rwyLen + 30
  const e = s.findIndex(x => !onRwy(x))
  if (e >= 0) { t.runway_clear_s = s[e].t - td.t; t.exit_at_m = s[e].along }
  const taxi = s.slice(slow)
  t.taxi_time_s = taxi.length ? taxi[taxi.length - 1].t - taxi[0].t : 0
  for (let i = 1; i < taxi.length; i++) t.taxi_distance_m += Math.hypot(taxi[i].x - taxi[i - 1].x, taxi[i].y - taxi[i - 1].y)
  t.max_taxi_kts = Math.max(0, ...taxi.map(x => x.gs_kts))
  let since: number | null = null, counted = false
  for (const x of taxi) {
    if (x.gs_kts < 1) {
      since ??= x.t
      if (!counted && x.t - since >= 3) { t.stops++; counted = true }
    } else if (x.gs_kts > 3) { since = null; counted = false }
  }
  if (t.max_taxi_kts > TAXI_LIMIT_KTS) t.calls.push(`fast taxi, ${Math.round(t.max_taxi_kts)} kt`)
  if (t.runway_clear_s == null) t.calls.push('never cleared the runway')
  else if (t.runway_clear_s > 60) t.calls.push(`slow to clear the runway, ${Math.round(t.runway_clear_s)} s`)
  if (end === 'stopped_on_runway') t.calls.push('stopped on the runway')
  return t
}
