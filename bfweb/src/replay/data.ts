// Flight replay data: the recording's meta, its five-minute track chunks,
// and interpolation. Mirrors bfdb/src/replay/ingest.rs -- see the chunk
// layout documented there.

import { API_ROOT } from '../api'
import { shortNameOf } from '../../../shared/models/catalog'
import { deadline, LONG_TIMEOUT_MS } from '@shared/web/api'

export type Kind =
  | 'air' | 'helo' | 'missile' | 'bomb' | 'rocket' | 'torpedo'
  | 'sam' | 'armor' | 'vehicle' | 'infantry' | 'static' | 'ship' | 'carrier'
  // Ingest v4 keeps what Tacview shows and v3 dropped at the door:
  | 'shell' | 'flare' | 'chaff' | 'decoy' | 'explosion' | 'parachutist' | 'bullseye' | 'misc'

export interface RecObject {
  k: Kind
  /** DCS unit type, e.g. "F-16C_50" */
  n?: string | null
  p?: string | null
  g?: string | null
  /** Tacview colour: "Blue", "Red", ... */
  c?: string | null
  cs?: string | null
  co?: string | null
  t0: number
  t1: number
  /** launcher, for weapons */
  par?: number | null
  f: number
  /** ms it was destroyed at */
  d?: number | null
  /** Tacview's own `Type=` tags, e.g. "Air+FixedWing" (ingest v4) */
  ty?: string | null
  /** Tacview object id, hex (ingest v4) */
  id?: string | null
}

export type EventKind =
  | 'fired' | 'kill' | 'destroyed' | 'lost' | 'takeoff' | 'landing'
  // Tacview's own `Event=` lines, passed through (ingest v4)
  | 'message' | 'bookmark' | 'hit' | 'left'

export interface RecEvent {
  t: number
  k: EventKind
  /** subject: shooter for "fired", victim for "kill"/"destroyed" */
  o?: number | null
  by?: number | null
  w?: number | null
  /** "fired": the enemy the weapon came closest to, how close (m), whether
   *  that was within lethal range, and when the weapon ended. */
  tg?: number | null
  md?: number | null
  hit?: boolean
  te?: number
  /** the text of a message/bookmark event */
  text?: string | null
}

export interface RecFlight {
  i: number
  pilot: string
  aircraft: string
  kind: string
  color?: string | null
  group?: string | null
  t0: number
  t1: number
  start_ms: number
  end_ms: number
  shots: number
  /** absent in recordings processed before hits were derived */
  hits?: number
  kills: number
  fate: 'destroyed' | 'landed' | 'left'
}

export interface RecMeta {
  v: number
  title?: string | null
  reference_time?: string | null
  recording_time?: string | null
  first_frame_s: number
  start_ms: number
  duration_ms: number
  chunk_ms: number
  chunks: number
  objects: RecObject[]
  events: RecEvent[]
  flights: RecFlight[]
}

/** As listed by /api/replay/recordings. */
export interface RecSummary {
  id: string
  instance: string
  file: string
  title?: string | null
  start_ms: number
  duration_ms: number
  objects: number
  chunks: number
  flights: number
  pilots: number
}

/** As listed by /api/replay/pilot/:ucid. */
export interface FlightRow {
  rec: string
  instance: string
  i: number
  pilot: string
  ucid?: string | null
  aircraft: string
  kind: string
  color?: string | null
  group?: string | null
  t0: number
  t1: number
  start_ms: number
  end_ms: number
  shots: number
  kills: number
  fate: 'destroyed' | 'landed' | 'left'
}

export interface ReplayStatus {
  instances: { instance: string; tacview: boolean; recordings: number; pending: number; failed: { file: string; error: string }[] }[]
  retention_days: number
}

export const F_ATT = 1, F_IAS = 2, F_MACH = 4, F_AOA = 8, F_AGL = 16

/** One object's samples within one chunk (or a stitched whole track). */
export interface Seg {
  /**
   * Every other numeric Tacview property the object carries (ingest v4),
   * one value per sample, carried forward between updates: "TAS", "HDG",
   * "FuelWeight", "Throttle", "RadarMode", "RadarRange", "Radius", ...
   * `LockedTarget`..`LockedTarget9` hold the target's meta index, -1 for none.
   */
  ch?: Record<string, Float64Array>
  t: Float64Array
  lon: Float64Array
  lat: Float64Array
  alt: Float64Array
  roll?: Float64Array
  pitch?: Float64Array
  yaw?: Float64Array
  ias?: Float64Array
  mach?: Float64Array
  aoa?: Float64Array
  agl?: Float64Array
}

/**
 * Decode a track chunk into object index -> segment. `BFR1` is ingest v3;
 * `BFR2` (v4) adds a channel table:
 *
 * ```text
 * "BFR2" u16:nchan, per channel: u8:len, utf8 name, f32:scale
 * u32:nobj
 * per object: u32:idx u32:n u8:flags u16:nch, nch x u16:channel
 *   t, lon, lat, alt [ATT roll pitch yaw] [IAS] [MACH] [AOA] [AGL]  as BFR1
 *   then each listed channel: n i32, delta-coded, value = int * scale
 * ```
 */
export function decodeChunk(buf: ArrayBuffer): Map<number, Seg> {
  const dv = new DataView(buf)
  const out = new Map<number, Seg>()
  const magic = buf.byteLength >= 8 ? dv.getUint32(0, true) : 0
  const v2 = magic === 0x32524642 /* "BFR2" */
  if (!v2 && magic !== 0x31524642 /* "BFR1" */) throw new Error('not a replay chunk')
  let at = 4
  const chans: { name: string; scale: number }[] = []
  if (v2) {
    const nchan = dv.getUint16(at, true)
    at += 2
    const td = new TextDecoder()
    for (let c = 0; c < nchan; c++) {
      const len = dv.getUint8(at)
      const name = td.decode(new Uint8Array(buf, at + 1, len))
      const scale = dv.getFloat32(at + 1 + len, true)
      chans.push({ name, scale })
      at += 1 + len + 4
    }
  }
  const nobj = dv.getUint32(at, true)
  at += 4
  for (let o = 0; o < nobj; o++) {
    const idx = dv.getUint32(at, true)
    const n = dv.getUint32(at + 4, true)
    const flags = dv.getUint8(at + 8)
    at += 9
    const mine: number[] = []
    if (v2) {
      const nch = dv.getUint16(at, true)
      at += 2
      for (let c = 0; c < nch; c++) { mine.push(dv.getUint16(at, true)); at += 2 }
    }
    const col = (scale: number): Float64Array => {
      const a = new Float64Array(n)
      let acc = 0
      for (let i = 0; i < n; i++) {
        acc += dv.getInt32(at, true)
        at += 4
        a[i] = acc * scale
      }
      return a
    }
    const seg: Seg = { t: col(1), lon: col(1e-7), lat: col(1e-7), alt: col(0.1) }
    if (flags & F_ATT) { seg.roll = col(0.01); seg.pitch = col(0.01); seg.yaw = col(0.01) }
    if (flags & F_IAS) seg.ias = col(0.01)
    if (flags & F_MACH) seg.mach = col(0.001)
    if (flags & F_AOA) seg.aoa = col(0.01)
    if (flags & F_AGL) seg.agl = col(0.1)
    if (mine.length) {
      seg.ch = {}
      for (const c of mine) {
        const ch = chans[c]
        if (!ch) throw new Error('bad channel in replay chunk')
        seg.ch[ch.name] = col(ch.scale)
      }
    }
    out.set(idx, seg)
  }
  return out
}

async function fetchBin(path: string): Promise<ArrayBuffer> {
  // Track blocks can be large on a slow link: the long deadline.
  const res = await fetch(`${API_ROOT}/api/replay${path}`, { credentials: 'include', signal: deadline(LONG_TIMEOUT_MS) })
  if (!res.ok) {
    let msg = `HTTP ${res.status}`
    try { const j = await res.json(); if (j?.error) msg = j.error } catch { /* not JSON */ }
    throw new Error(msg)
  }
  return res.arrayBuffer()
}

export async function fetchMeta(rec: string): Promise<RecMeta> {
  const res = await fetch(`${API_ROOT}/api/replay/rec/${encodeURIComponent(rec)}`, { credentials: 'include', signal: deadline() })
  if (!res.ok) {
    let msg = `HTTP ${res.status}`
    try { const j = await res.json(); if (j?.error) msg = j.error } catch { /* not JSON */ }
    throw new Error(msg)
  }
  return res.json()
}

/** One object's whole track, stitched across chunks (null if the recording
 *  holds no samples for it). */
export async function fetchObjectTrack(rec: string, meta: RecMeta, idx: number): Promise<Seg | null> {
  const o = meta.objects[idx]
  if (!o) return null
  const buf = await fetchBin(`/rec/${encodeURIComponent(rec)}/object/${idx}?t0=${o.t0}&t1=${o.t1}`)
  return decodeChunk(buf).get(idx) ?? null
}

/** Meta indices of the flights flown by players (the rest are AI). */
/** A training-range result graded during a recording (bfdb `/api/range/replay/<rec>/results`). */
export interface RangeHit {
  id: string
  kind: string
  pilot: string
  ucid: string | null
  unit_type: string
  /** ms from the recording start */
  t: number
  headline: string | null
  score: number | null
}

/** The range results in a recording, and where the range site lives; empty when it has none. */
export async function fetchRangeHits(rec: string): Promise<{ items: RangeHit[]; site: string }> {
  const res = await fetch(`${API_ROOT}/api/range/replay/${encodeURIComponent(rec)}/results`, { credentials: 'include', signal: deadline() })
  if (!res.ok) return { items: [], site: '' }
  return res.json()
}

export async function fetchHumans(rec: string): Promise<number[]> {
  const res = await fetch(`${API_ROOT}/api/replay/rec/${encodeURIComponent(rec)}/humans`, { credentials: 'include', signal: deadline() })
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  return res.json()
}

/** A sampled state. Angles in degrees, speeds m/s, altitudes m MSL. */
export interface State {
  lon: number
  lat: number
  alt: number
  roll: number
  pitch: number
  /** true heading of the nose (or of travel, for objects without attitude) */
  hdg: number
  /** ground speed, from the track */
  gs: number
  /** vertical speed */
  vs: number
  /** climb angle and track of the flight path, smoothed across samples
   *  (absent when standing still) */
  path?: { pitch: number; trk: number }
  ias?: number
  mach?: number
  aoa?: number
  agl?: number
  /** the segment's extra channels at this moment (see `Seg.ch`) */
  ch?: Record<string, number>
}

/** Channels that jump between values rather than blend: target ids, modes,
 *  switch positions. */
export function isStepChannel(name: string): boolean {
  return /^LockedTarget\d?$|Mode$|^LandingGear|^Tailhook|^Parachute|^DragChute|^Afterburner|^Visible|^Disabled|^Importance/.test(name)
}

const lerp = (a: number, b: number, f: number) => a + (b - a) * f
function lerpAngle(a: number, b: number, f: number): number {
  const d = ((b - a + 540) % 360) - 180
  return (a + d * f + 360) % 360
}
const R = 6371008.8
export function distM(lon1: number, lat1: number, lon2: number, lat2: number): number {
  const p = Math.PI / 180
  const x = (lon2 - lon1) * p * Math.cos(((lat1 + lat2) / 2) * p)
  const y = (lat2 - lat1) * p
  return Math.sqrt(x * x + y * y) * R
}
export function bearingDeg(lon1: number, lat1: number, lon2: number, lat2: number): number {
  const p = Math.PI / 180
  const y = Math.sin((lon2 - lon1) * p) * Math.cos(lat2 * p)
  const x = Math.cos(lat1 * p) * Math.sin(lat2 * p) - Math.sin(lat1 * p) * Math.cos(lat2 * p) * Math.cos((lon2 - lon1) * p)
  return (Math.atan2(y, x) / p + 360) % 360
}

const DEG_PER_RAD = 180 / Math.PI
/** Velocity at sample `k`, m/s east, north, up, from its neighbours. */
function velAt(s: Seg, k: number): [number, number, number] {
  const n = s.t.length
  const a = Math.max(k - 1, 0), b = Math.min(k + 1, n - 1)
  const dt = (s.t[b] - s.t[a]) / 1000
  if (!(dt > 0)) return [0, 0, 0]
  const p = Math.PI / 180
  const e = (s.lon[b] - s.lon[a]) * p * Math.cos(((s.lat[a] + s.lat[b]) / 2) * p) * R
  const no = (s.lat[b] - s.lat[a]) * p * R
  return [e / dt, no / dt, (s.alt[b] - s.alt[a]) / dt]
}

/** Index of the last sample at or before `t` (-1 if none). */
export function bisect(ts: Float64Array, t: number): number {
  let lo = 0, hi = ts.length - 1
  if (hi < 0 || t < ts[0]) return -1
  if (t >= ts[hi]) return hi
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1
    if (ts[mid] <= t) lo = mid
    else hi = mid - 1
  }
  return lo
}

/** Interpolate a segment at `t`. Holds the last sample past the end. */
export function sampleSeg(s: Seg, t: number): State | null {
  const n = s.t.length
  if (n === 0) return null
  const i = bisect(s.t, t)
  if (i < 0) return null
  const j = Math.min(i + 1, n - 1)
  // Rates come from the bracketing pair, or the last pair when holding.
  const a = j === i && i > 0 ? i - 1 : i
  const b = j === i && i > 0 ? i : j
  const f = j === i ? 0 : (t - s.t[i]) / (s.t[j] - s.t[i] || 1)
  const dt = (s.t[b] - s.t[a]) / 1000
  const moved = a !== b ? distM(s.lon[a], s.lat[a], s.lon[b], s.lat[b]) : 0
  const gs = dt > 0 ? moved / dt : 0
  const vs = dt > 0 ? (s.alt[b] - s.alt[a]) / dt : 0
  const hdg = s.yaw
    ? lerpAngle(s.yaw[i], s.yaw[j], f)
    : moved > 0.5 ? bearingDeg(s.lon[a], s.lat[a], s.lon[b], s.lat[b]) : 0
  const st: State = {
    lon: lerp(s.lon[i], s.lon[j], f),
    lat: lerp(s.lat[i], s.lat[j], f),
    alt: lerp(s.alt[i], s.alt[j], f),
    roll: s.roll ? lerp(s.roll[i], s.roll[j], f) : 0,
    pitch: s.pitch ? lerp(s.pitch[i], s.pitch[j], f) : 0,
    hdg,
    gs,
    vs,
  }
  // The velocity at each sample (central difference), blended between the
  // two around `t` so the path angle turns smoothly instead of stepping.
  const vi = velAt(s, i), vj = j === i ? vi : velAt(s, j)
  const ve = lerp(vi[0], vj[0], f), vn = lerp(vi[1], vj[1], f), vu = lerp(vi[2], vj[2], f)
  const flat = Math.hypot(ve, vn)
  if (flat + Math.abs(vu) > 1) {
    st.path = { pitch: Math.atan2(vu, flat) * DEG_PER_RAD, trk: (Math.atan2(ve, vn) * DEG_PER_RAD + 360) % 360 }
  }
  if (s.ias) st.ias = lerp(s.ias[i], s.ias[j], f)
  if (s.mach) st.mach = lerp(s.mach[i], s.mach[j], f)
  if (s.aoa) st.aoa = lerp(s.aoa[i], s.aoa[j], f)
  if (s.agl) st.agl = lerp(s.agl[i], s.agl[j], f)
  if (s.ch) {
    st.ch = {}
    for (const k in s.ch) {
      const c = s.ch[k]
      st.ch[k] = isStepChannel(k) ? c[f < 1 ? i : j] : lerp(c[i], c[j], f)
    }
  }
  return st
}

/**
 * What the viewer plays: a server recording streamed in chunks
 * (`TrackStore`) or a local .acmi read whole in the browser
 * (`acmi/store.ts`). Times are ms from the recording start.
 */
export interface TrackSource {
  readonly meta: RecMeta
  /** bumps whenever more data arrives, so views redraw what they cache */
  version: number
  onChange: (() => void) | null
  error: string | null
  /** fetch what is needed around the playhead (no-op when all is loaded) */
  around(t: number): void
  /** whether the data under `t` is here */
  isLoaded(t: number): boolean
  /** meta indices that may be alive at `t` (callers still check t0/t1) */
  present(t: number): number[]
  /** interpolated state, or null when not alive or not loaded */
  state(idx: number, t: number): State | null
  /** the whole track (null until it arrives; asking starts the fetch) */
  wholeTrack(idx: number): Seg | null
  /** [lon, lat, alt] points from t-len to t, at most one per `step` ms */
  trail(idx: number, t: number, len: number, step?: number): [number, number, number][]
}

/**
 * The loaded part of a recording. Chunks are fetched on demand around the
 * playhead; `version` bumps whenever something new arrives so views know to
 * redraw what is static (trails).
 */
export class TrackStore implements TrackSource {
  readonly meta: RecMeta
  readonly rec: string
  private chunks = new Map<number, Map<number, Seg>>()
  private loading = new Map<number, Promise<void>>()
  private whole = new Map<number, Seg | null>()
  private wholeLoading = new Set<number>()
  version = 0
  onChange: (() => void) | null = null
  error: string | null = null

  constructor(rec: string, meta: RecMeta) {
    this.rec = rec
    this.meta = meta
  }

  chunkOf(t: number): number {
    return Math.max(0, Math.min(this.meta.chunks - 1, Math.floor(t / this.meta.chunk_ms)))
  }

  hasChunk(n: number): boolean {
    return this.chunks.has(n)
  }

  /** Make sure chunk `n` is (being) loaded. */
  ensure(n: number): Promise<void> {
    if (n < 0 || n >= this.meta.chunks || this.chunks.has(n)) return Promise.resolve()
    let p = this.loading.get(n)
    if (!p) {
      p = fetchBin(`/rec/${encodeURIComponent(this.rec)}/chunk/${n}`)
        .then(buf => {
          this.chunks.set(n, decodeChunk(buf))
          this.version++
          this.onChange?.()
        })
        .catch(e => { this.error = String(e?.message ?? e) })
        .finally(() => this.loading.delete(n))
      this.loading.set(n, p)
    }
    return p
  }

  /** Load the chunk under `t` and the next one; drop far-away chunks. */
  around(t: number) {
    const n = this.chunkOf(t)
    this.ensure(n)
    this.ensure(n + 1)
    if (this.chunks.size > 8) {
      for (const k of [...this.chunks.keys()]) {
        if (k < n - 2 || k > n + 3) this.chunks.delete(k)
      }
    }
  }

  isLoaded(t: number): boolean {
    return this.chunks.has(this.chunkOf(t))
  }

  /** One object's stitched whole track (for trails, graphs); null until
   *  fetched, and fetched once. */
  wholeTrack(idx: number): Seg | null {
    if (this.whole.has(idx)) return this.whole.get(idx) ?? null
    if (!this.wholeLoading.has(idx)) {
      this.wholeLoading.add(idx)
      const o = this.meta.objects[idx]
      fetchBin(`/rec/${encodeURIComponent(this.rec)}/object/${idx}?t0=${o.t0}&t1=${o.t1}`)
        .then(buf => {
          this.whole.set(idx, decodeChunk(buf).get(idx) ?? null)
          this.version++
          this.onChange?.()
        })
        .catch(() => this.whole.set(idx, null))
    }
    return null
  }

  /** The segment holding `idx` at time `t`, if loaded. */
  seg(idx: number, t: number): Seg | undefined {
    return this.chunks.get(this.chunkOf(t))?.get(idx)
  }

  state(idx: number, t: number): State | null {
    const o = this.meta.objects[idx]
    if (!o || t < o.t0 || t > o.t1) return null
    const w = this.whole.get(idx)
    if (w) return sampleSeg(w, t)
    const s = this.seg(idx, t)
    return s ? sampleSeg(s, t) : null
  }

  /** Objects present in the chunk under `t`. */
  present(t: number): number[] {
    const c = this.chunks.get(this.chunkOf(t))
    return c ? [...c.keys()] : []
  }

  /** [lon, lat, alt] points of `idx` between t-len and t, from what is
   *  loaded (the whole track when we have it). */
  trail(idx: number, t: number, len: number, step = 0): [number, number, number][] {
    const w = this.whole.get(idx)
    const segs: Seg[] = []
    if (w) segs.push(w)
    else {
      for (let n = this.chunkOf(t - len); n <= this.chunkOf(t); n++) {
        const s = this.chunks.get(n)?.get(idx)
        if (s) segs.push(s)
      }
    }
    return trailOf(segs, t, len, step, this.state(idx, t))
  }
}

/** [lon, lat, alt] points of `segs` (in time order) between t-len and t, at
 *  most one per `step` ms, ending at `now`. Starts by bisection: a whole
 *  track holds hours of samples and this runs every frame. */
export function trailOf(segs: Seg[], t: number, len: number, step: number, now: State | null): [number, number, number][] {
  const pts: [number, number, number][] = []
  let lastT = -Infinity
  for (const s of segs) {
    for (let i = Math.max(0, bisect(s.t, t - len)); i < s.t.length; i++) {
      const ti = s.t[i]
      if (ti < t - len || ti <= lastT) continue
      if (ti > t) break
      if (step && ti - lastT < step) continue
      pts.push([s.lon[i], s.lat[i], s.alt[i]])
      lastT = ti
    }
  }
  if (now) pts.push([now.lon, now.lat, now.alt])
  return pts
}

// ── shots ──────────────────────────────────────────────────────────────

export interface Shot {
  ev: RecEvent
  result: 'kill' | 'hit' | 'miss' | 'unknown'
}

/** Every "fired" event, with its outcome. */
export function shotsOf(meta: RecMeta): Shot[] {
  const killed = new Set<number>()
  for (const e of meta.events) if (e.k === 'kill' && e.w != null) killed.add(e.w)
  return meta.events
    .filter(e => e.k === 'fired')
    .map(ev => ({
      ev,
      result: ev.w != null && killed.has(ev.w) ? 'kill' : ev.hit ? 'hit' : ev.md != null ? 'miss' : 'unknown',
    }))
}

// ── presentation helpers ───────────────────────────────────────────────

export const SIDE_HEX: Record<string, string> = {
  Blue: '#4a8fd4',
  Red: '#d24b4b',
}
export function sideHex(c?: string | null): string {
  return (c && SIDE_HEX[c]) || '#c9a227'
}

export function isAir(k: Kind) { return k === 'air' || k === 'helo' }
export function isWeapon(k: Kind) { return k === 'missile' || k === 'bomb' || k === 'rocket' || k === 'torpedo' }
/** Shells, flares, chaff, decoys, explosions: drawn as effects, never a target. */
export function isEffect(k: Kind) { return k === 'shell' || k === 'flare' || k === 'chaff' || k === 'decoy' || k === 'explosion' }
/** Ground units, ships and statics. */
export function isSurface(k: Kind) { return !isAir(k) && !isWeapon(k) && !isEffect(k) && k !== 'parachutist' && k !== 'bullseye' && k !== 'misc' }

/** The type as Tacview names it ("F-16C" for "F-16C_50"), else the raw name. */
export function typeLabel(o: RecObject): string {
  return shortNameOf(o.n) || (o.n ?? o.k).replace(/_/g, ' ')
}

export function objLabel(o: RecObject): string {
  const type = typeLabel(o)
  return o.p ? `${o.p} (${type})` : type
}

export const M_TO_FT = 3.28084
export const MS_TO_KT = 1.943844

export function fmtDuration(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000))
  const h = Math.floor(s / 3600), m = Math.floor((s % 3600) / 60), r = s % 60
  return h ? `${h}:${String(m).padStart(2, '0')}:${String(r).padStart(2, '0')}` : `${m}:${String(r).padStart(2, '0')}`
}

export function fmtClock(unixMs: number): string {
  return new Date(unixMs).toISOString().slice(11, 19) + 'Z'
}

/** In-game clock at recording-relative `t`. */
export function gameClock(meta: RecMeta, t: number): string | null {
  if (!meta.reference_time) return null
  const base = Date.parse(meta.reference_time)
  if (Number.isNaN(base)) return null
  return new Date(base + meta.first_frame_s * 1000 + t).toISOString().slice(11, 19)
}
