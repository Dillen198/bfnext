// Turn a Tacview recording into replay data in the browser: the TypeScript
// port of the ingest v4 Builder in bfdb/src/replay/ingest.rs. Where that one
// writes five-minute chunks to disk, this keeps every track whole in memory.
//
// Read once, front to back. Per object: growable typed-array columns
// (Float64 for t/lon/lat, Float32 for the rest while building), thinned per
// kind, with the newest unwritten state always kept when the object leaves
// or the recording ends. Derived on the way: launchers, each weapon's
// closest approach to an enemy, takeoffs and landings, and at the end hits,
// kills, losses and flights. Imports types only from ../data, so the worker
// bundle does not pull in the API client.

import type { Kind, RecEvent, RecFlight, RecMeta, RecObject, Seg } from '../data'
import { AcmiParser, idHex, parseId, parseTransform, type AcmiSink, type Id, type Props } from './parse'

export const F_ATT = 1, F_IAS = 2, F_MACH = 4, F_AOA = 8, F_AGL = 16

/** What one recording becomes: the meta and every whole track by meta index. */
export interface AcmiData {
  meta: RecMeta & { author?: string | null; data_source?: string | null }
  tracks: Map<number, Seg>
}

export interface FinishOptions {
  /**
   * Unix ms the recording ended at, used for `start_ms` when it has no
   * `RecordingTime` (bfdb uses the file's mtime; a dropped File has
   * `lastModified`).
   */
  fileEndMs?: number
}

/** Map a Tacview `Type=` tag list to a kind; null drops the object (only
 *  `Shrapnel` and `Beam`). Countermeasures before `Decoy`, shells before
 *  `Weapon`, parachutists before `Air` (DCS types them `...+Air+Parachutist`). */
export function classify(ty: string): Kind | null {
  const tags = ty.split('+').map(s => s.trim())
  const has = (t: string) => tags.includes(t)
  if (has('Shrapnel') || has('Beam')) return null
  if (has('Flare')) return 'flare'
  if (has('Chaff')) return 'chaff'
  if (has('Decoy')) return 'decoy'
  if (has('Shell') || has('Bullet') || has('Projectile') || has('Grenade')) return 'shell'
  if (has('Explosion')) return 'explosion'
  if (has('Parachutist')) return 'parachutist'
  if (has('Bullseye')) return 'bullseye'
  if (has('Air')) return has('Rotorcraft') ? 'helo' : 'air'
  if (has('Weapon')) {
    if (has('Missile')) return 'missile'
    if (has('Bomb')) return 'bomb'
    if (has('Rocket')) return 'rocket'
    if (has('Torpedo')) return 'torpedo'
    return 'misc'
  }
  if (has('Ground')) {
    if (has('AntiAircraft')) return 'sam'
    if (has('Static') || has('Building') || has('Aerodrome') || has('Container')) return 'static'
    if (has('Tank') || has('Armor')) return 'armor'
    if (has('Infantry') || has('Human')) return 'infantry'
    return 'vehicle'
  }
  if (has('Sea')) return has('AircraftCarrier') ? 'carrier' : 'ship'
  return 'misc'
}

/** Minimum spacing of stored samples (ms). */
const INTERVAL: Record<Kind, number> = {
  air: 100, helo: 100, missile: 100, bomb: 100, rocket: 100, torpedo: 100,
  ship: 1000, carrier: 1000, sam: 1000, armor: 1000, vehicle: 1000, infantry: 1000,
  flare: 500, chaff: 500, decoy: 500,
  shell: 1000, parachutist: 1000, misc: 1000,
  static: Infinity, explosion: Infinity, bullseye: Infinity,
}

const isWeapon = (k: Kind | null) => k === 'missile' || k === 'bomb' || k === 'rocket' || k === 'torpedo'
const isAir = (k: Kind | null) => k === 'air' || k === 'helo'
const isEffect = (k: Kind | null) =>
  k === 'shell' || k === 'flare' || k === 'chaff' || k === 'decoy' || k === 'explosion' ||
  k === 'parachutist' || k === 'bullseye' || k === 'misc'
/** What a weapon can be fired at or from: aircraft, ground units, ships, statics. */
const isTarget = (k: Kind | null) => k !== null && !isWeapon(k) && !isEffect(k)

function lethalRadiusM(k: Kind): number {
  return k === 'missile' ? 75 : k === 'bomb' || k === 'torpedo' ? 40 : 25
}

/** Text properties: never channels, even when the value happens to be a number. */
const TEXT = new Set([
  'Name', 'Type', 'Parent', 'Next', 'Pilot', 'Group', 'Color', 'Coalition', 'Country', 'CallSign',
  'Registration', 'Squadron', 'FlightNumber', 'Label', 'ShortName', 'LongName', 'FullName', 'AltName',
  'Rank', 'ICAO24', 'FocusedTarget', 'Debug', 'Event',
])

/** `LockedTarget`, `LockedTarget2` .. `LockedTarget9`: object ids. */
function isLock(k: string): boolean {
  if (!k.startsWith('LockedTarget')) return false
  if (k.length === 12) return true
  if (k.length !== 13) return false
  const c = k.charCodeAt(12)
  return c >= 48 && c <= 57
}

interface Chan {
  cur: number
  col: Float32Array | Float64Array
  /** holds a sid while building, a meta index once finished */
  lock: boolean
}

class Obj {
  readonly id: Id
  kind: Kind | null = null
  typed = false
  skip = false
  ty: string | null = null
  name: string | null = null
  pilot: string | null = null
  group: string | null = null
  color: string | null = null
  callsign: string | null = null
  country: string | null = null
  parentTid: Id | null = null
  /** launcher sid, -1 for none */
  launcher = -1
  t0: number
  t1: number
  // current state
  t = 0; lon = 0; lat = 0; alt = 0; roll = 0; pitch = 0; yaw = 0
  ias = 0; mach = 0; aoa = 0; agl = 0
  positioned = false
  pending = false
  emitted = false
  lastEmit = -Infinity
  flags = 0
  alive = true
  inTargets = false
  /** Destroyed event: when, and where it was then */
  destroyedT = -1; dLon = 0; dLat = 0; dAlt = 0
  idx = -1
  // derived from the track
  hasPrev = false; prevT = 0; prevLon = 0; prevLat = 0
  gs = 0
  /** airborne: null until settled */
  air: boolean | null = null
  hasCand = false; candTo = false; candSince = 0
  scanT = -Infinity
  bestD = Infinity; bestSid = -1; bestT = 0
  removed = false
  removedAirborne = false
  // columns
  n = 0
  cap = 0
  ct: Float64Array = EMPTY64; clon: Float64Array = EMPTY64; clat: Float64Array = EMPTY64; calt: Float32Array = EMPTY32
  croll: Float32Array | null = null; cpitch: Float32Array | null = null; cyaw: Float32Array | null = null
  cias: Float32Array | null = null; cmach: Float32Array | null = null
  caoa: Float32Array | null = null; cagl: Float32Array | null = null
  chans: Map<string, Chan> | null = null

  constructor(id: Id, now: number) {
    this.id = id
    this.t0 = now
    this.t1 = now
  }

  interval(): number {
    return this.kind ? INTERVAL[this.kind] : 500
  }

  private grow() {
    const cap = Math.max(4, this.cap * 2)
    this.ct = grow64(this.ct, cap)
    this.clon = grow64(this.clon, cap)
    this.clat = grow64(this.clat, cap)
    this.calt = grow32(this.calt, cap)
    if (this.croll) { this.croll = grow32(this.croll, cap); this.cpitch = grow32(this.cpitch!, cap); this.cyaw = grow32(this.cyaw!, cap) }
    if (this.cias) this.cias = grow32(this.cias, cap)
    if (this.cmach) this.cmach = grow32(this.cmach, cap)
    if (this.caoa) this.caoa = grow32(this.caoa, cap)
    if (this.cagl) this.cagl = grow32(this.cagl, cap)
    if (this.chans) {
      for (const c of this.chans.values()) c.col = c.lock ? grow64(c.col as Float64Array, cap) : grow32(c.col as Float32Array, cap)
    }
    this.cap = cap
  }

  /** Set a channel; the first set fills the samples already written. */
  setChan(name: string, v: number, lock: boolean) {
    const chans = this.chans ??= new Map()
    const c = chans.get(name)
    if (c) { c.cur = v; return }
    const col = lock ? new Float64Array(this.cap) : new Float32Array(this.cap)
    // Before the first set: the first value, or "no lock" for a target.
    col.fill(lock ? -1 : v, 0, this.n)
    chans.set(name, { cur: v, col, lock })
  }

  /** Write the current state as a sample (over the last one when it is at
   *  the same instant). */
  emit() {
    let k = this.n
    if (k > 0 && this.ct[k - 1] === this.t) k--
    else {
      if (k === this.cap) this.grow()
      this.n++
    }
    const f = this.flags
    if (f & F_ATT && !this.croll) { this.croll = new Float32Array(this.cap); this.cpitch = new Float32Array(this.cap); this.cyaw = new Float32Array(this.cap) }
    if (f & F_IAS && !this.cias) this.cias = new Float32Array(this.cap)
    if (f & F_MACH && !this.cmach) this.cmach = new Float32Array(this.cap)
    if (f & F_AOA && !this.caoa) this.caoa = new Float32Array(this.cap)
    if (f & F_AGL && !this.cagl) this.cagl = new Float32Array(this.cap)
    this.ct[k] = this.t
    this.clon[k] = this.lon
    this.clat[k] = this.lat
    this.calt[k] = this.alt
    if (this.croll) { this.croll[k] = this.roll; this.cpitch![k] = this.pitch; this.cyaw![k] = this.yaw }
    if (this.cias) this.cias[k] = this.ias
    if (this.cmach) this.cmach[k] = this.mach
    if (this.caoa) this.caoa[k] = this.aoa
    if (this.cagl) this.cagl[k] = this.agl
    if (this.chans) for (const c of this.chans.values()) c.col[k] = c.cur
    this.lastEmit = this.t
    this.emitted = true
    this.pending = false
  }

  dropColumns() {
    this.n = this.cap = 0
    this.ct = this.clon = this.clat = EMPTY64
    this.calt = EMPTY32
    this.croll = this.cpitch = this.cyaw = this.cias = this.cmach = this.caoa = this.cagl = null
    this.chans = null
  }
}

const EMPTY64: Float64Array = new Float64Array(0)
const EMPTY32: Float32Array = new Float32Array(0)
function grow64(a: Float64Array, cap: number): Float64Array { const b = new Float64Array(cap); b.set(a); return b }
function grow32(a: Float32Array, cap: number): Float32Array { const b = new Float32Array(cap); b.set(a); return b }

function dist2dM(aLon: number, aLat: number, bLon: number, bLat: number): number {
  const lat = ((aLat + bLat) / 2) * (Math.PI / 180)
  const dx = (aLon - bLon) * 111_320 * Math.cos(lat)
  const dy = (aLat - bLat) * 110_540
  return Math.sqrt(dx * dx + dy * dy)
}

function distM(a: Obj, bLon: number, bLat: number, bAlt: number): number {
  const lat = ((a.lat + bLat) / 2) * (Math.PI / 180)
  const dx = (a.lon - bLon) * 111_320 * Math.cos(lat)
  const dy = (a.lat - bLat) * 110_540
  const dz = a.alt - bAlt
  return Math.sqrt(dx * dx + dy * dy + dz * dz)
}

type NavKind = 'destroyed' | 'takeoff' | 'landing'
interface Ev { t: number; k: NavKind; sid: number }
interface TacEv { t: number; k: 'message' | 'bookmark' | 'hit' | 'left'; sid: number; text: string | null }

/** Slabs of Float64 storage the finished columns are views into, so the
 *  worker hands back a few large buffers rather than one per column. */
class Slabs {
  private left: number
  private cur = new Float64Array(0)
  private used = 0
  constructor(total: number) { this.left = total }
  take(n: number): Float64Array {
    if (this.used + n > this.cur.length) {
      const size = Math.max(n, Math.min(this.left, 1 << 25))
      this.cur = new Float64Array(size)
      this.used = 0
    }
    const v = this.cur.subarray(this.used, this.used + n)
    this.used += n
    this.left -= n
    return v
  }
}

/** The ingest v4 builder. Feed it as an `AcmiSink`, then `finish()`. */
export class Builder implements AcmiSink {
  private refLon = 0
  private refLat = 0
  private referenceTime: string | null = null
  private recordingTime: string | null = null
  private title: string | null = null
  private author: string | null = null
  private dataSource: string | null = null
  private firstT: number | null = null
  private firstFrameS = 0
  private now = 0
  private objs: Obj[] = []
  private alive = new Map<Id, number>()
  /** alive, positioned sids that can be a target or a launcher */
  private targets = new Set<number>()
  private evs: Ev[] = []
  private tacEvs: TacEv[] = []
  private tf = new Float64Array(6)

  frame(t: number): void {
    if (this.firstT === null) { this.firstT = t; this.firstFrameS = t }
    const ms = Math.round((t - this.firstT) * 1000)
    // Time only goes forward; a stray earlier frame is held at "now".
    if (ms > this.now) this.now = ms
  }

  remove(tid: Id): void {
    const sid = this.alive.get(tid)
    if (sid === undefined) return
    const o = this.objs[sid]
    // A weapon's last look for its target, while everything near it is
    // still in the picture.
    if (isWeapon(o.kind) && o.positioned) this.scan(sid, true)
    this.alive.delete(tid)
    if (o.inTargets) { this.targets.delete(sid); o.inTargets = false }
    o.alive = false
    o.removed = true
    o.t1 = this.now
    // No AGL in the recording: flying if it was moving like it.
    o.removedAirborne = isAir(o.kind) && (o.air !== null ? o.air : o.gs > 50)
    if (o.pending && o.positioned && !o.skip) o.emit()
  }

  object(tid: Id, p: Props): void {
    if (tid === 0) { this.global(p); return }
    let sid = this.alive.get(tid)
    if (sid === undefined) {
      sid = this.objs.length
      this.objs.push(new Obj(tid, this.now))
      this.alive.set(tid, sid)
    }
    const o = this.objs[sid]
    if (o.skip) return
    const now = this.now
    const tf = this.tf
    let hasT = false, att = false, moved = false
    while (p.next()) {
      const k = p.key
      switch (k) {
        case 'T':
          if (p.esc) { const v = p.value(); att = parseTransform(v, 0, v.length, tf) }
          else att = parseTransform(p.src, p.vs, p.ve, tf)
          hasT = true
          break
        case 'Type': {
          o.typed = true
          o.ty = p.value()
          o.kind = classify(o.ty)
          if (o.kind === null) {
            o.skip = true
            o.dropColumns()
            if (o.inTargets) { this.targets.delete(sid); o.inTargets = false }
            return
          }
          if (o.inTargets && !isTarget(o.kind)) { this.targets.delete(sid); o.inTargets = false }
          break
        }
        case 'Name': o.name = p.value(); break
        case 'Pilot': o.pilot = p.value(); break
        case 'Group': o.group = p.value(); break
        case 'Color': o.color = p.value(); break
        case 'CallSign': o.callsign = p.value(); break
        case 'Country': o.country = p.value(); break
        case 'Parent': o.parentTid = p.id(); break
        case 'IAS': case 'Mach': case 'AOA': case 'AGL': {
          const x = p.num()
          if (Number.isNaN(x)) break
          if (k === 'IAS') { o.ias = x; o.flags |= F_IAS }
          else if (k === 'Mach') { o.mach = x; o.flags |= F_MACH }
          else if (k === 'AOA') { o.aoa = x; o.flags |= F_AOA }
          else { o.agl = x; o.flags |= F_AGL }
          moved = true
          break
        }
        default:
          if (isLock(k)) {
            const id = p.id()
            const target = id === null ? undefined : this.alive.get(id)
            o.setChan(k, target === undefined ? -1 : target, true)
            moved = true
          } else if (!TEXT.has(k)) {
            const x = p.num()
            if (!Number.isNaN(x)) { o.setChan(k, x, false); moved = true }
          }
      }
    }
    if (hasT) {
      if (!Number.isNaN(tf[0])) o.lon = this.refLon + tf[0]
      if (!Number.isNaN(tf[1])) o.lat = this.refLat + tf[1]
      if (!Number.isNaN(tf[2])) o.alt = tf[2]
      if (att) {
        o.flags |= F_ATT
        if (!Number.isNaN(tf[3])) o.roll = tf[3]
        if (!Number.isNaN(tf[4])) o.pitch = tf[4]
        if (!Number.isNaN(tf[5])) o.yaw = tf[5]
      }
      moved = true
    }
    if (!moved) return
    const firstFix = !o.positioned && hasT
    if (hasT) o.positioned = true
    if (!o.positioned) return
    o.t = now
    o.pending = true
    // A second line for the same object in one frame updates that sample.
    if (!o.emitted || now - o.lastEmit >= o.interval() || o.lastEmit === now) o.emit()
    const kind = o.kind
    if (!o.inTargets && isTarget(kind)) { this.targets.add(sid); o.inTargets = true }
    if (firstFix && isWeapon(kind)) this.findLauncher(sid)
    if (hasT) {
      if (isAir(kind)) this.airState(sid)
      else if (isWeapon(kind)) this.scan(sid, false)
    }
  }

  private global(p: Props) {
    while (p.next()) {
      switch (p.key) {
        case 'ReferenceLongitude': { const x = p.num(); this.refLon = Number.isNaN(x) ? 0 : x; break }
        case 'ReferenceLatitude': { const x = p.num(); this.refLat = Number.isNaN(x) ? 0 : x; break }
        case 'ReferenceTime': this.referenceTime = p.value(); break
        case 'RecordingTime': this.recordingTime = p.value(); break
        case 'Title': this.title = p.value(); break
        case 'Author': this.author = p.value(); break
        case 'DataSource': this.dataSource = p.value(); break
        case 'Event': this.event(p.value()); break
      }
    }
  }

  private lookup(s: string | undefined): number {
    if (s === undefined) return -1
    const id = parseId(s, 0, s.length)
    return id === null ? -1 : this.alive.get(id) ?? -1
  }

  /** `Type|id|id|...|text` */
  private event(v: string) {
    const parts = v.split('|')
    const ty = parts[0]
    const now = this.now
    const last = parts.length >= 2 ? parts[parts.length - 1] : ''
    switch (ty) {
      case 'Destroyed': case 'TakenOff': case 'Landed': {
        const sid = this.lookup(parts[1])
        if (sid < 0) return
        if (ty === 'Destroyed') {
          const o = this.objs[sid]
          if (o.destroyedT >= 0) return
          o.destroyedT = now; o.dLon = o.lon; o.dLat = o.lat; o.dAlt = o.alt
        }
        this.evs.push({ t: now, k: ty === 'Destroyed' ? 'destroyed' : ty === 'TakenOff' ? 'takeoff' : 'landing', sid })
        return
      }
      case 'Message': case 'Bookmark': {
        // `Bookmark|text` has no ids; `Message|id|text` names one.
        const sid = parts.length >= 3 ? this.lookup(parts[1]) : -1
        this.tacEvs.push({ t: now, k: ty === 'Message' ? 'message' : 'bookmark', sid, text: last || null })
        return
      }
      case 'Hit': case 'LeftArea': {
        const sid = this.lookup(parts[1])
        this.tacEvs.push({ t: now, k: ty === 'Hit' ? 'hit' : 'left', sid, text: parts.length >= 3 ? last || null : null })
        return
      }
    }
  }

  /** Track an aircraft's ground speed and whether it is flying; a change
   *  that holds for 3 s is a takeoff or a landing. */
  private airState(sid: number) {
    const now = this.now
    const o = this.objs[sid]
    if (!o.hasPrev) {
      o.hasPrev = true; o.prevT = now; o.prevLon = o.lon; o.prevLat = o.lat
    } else if (now - o.prevT >= 500) {
      o.gs = dist2dM(o.prevLon, o.prevLat, o.lon, o.lat) / ((now - o.prevT) / 1000)
      o.prevT = now; o.prevLon = o.lon; o.prevLat = o.lat
    }
    if (!(o.flags & F_AGL)) return
    const helo = o.kind === 'helo'
    const agl = o.agl, gs = o.gs
    const up = helo ? agl > 8 : agl > 15 && gs > 25
    const down = helo ? agl < 3.5 && gs < 6 : agl < 4 && gs < 40
    if (o.air === null) {
      if (up) o.air = true
      else if (down) o.air = false
      return
    }
    const wants = o.air ? down : up
    if (!wants) { o.hasCand = false; return }
    if (!o.hasCand) { o.hasCand = true; o.candTo = !o.air; o.candSince = now; return }
    if (now - o.candSince >= 3000) {
      o.air = o.candTo
      o.hasCand = false
      this.evs.push({ t: o.candSince, k: o.candTo ? 'takeoff' : 'landing', sid })
    }
  }

  /** A weapon's closest approach to anything not on its side: every second
   *  while far from everything, every update once within 3 km. */
  private scan(sid: number, last: boolean) {
    const now = this.now
    const w = this.objs[sid]
    const close = w.bestD < 3000
    if (!last && !close && now - w.scanT < 1000) return
    const launcher = w.launcher
    const side = w.color ?? (launcher >= 0 ? this.objs[launcher].color : null)
    let bd = Infinity, bc = -1
    for (const c of this.targets) {
      if (c === sid || c === launcher) continue
      const o = this.objs[c]
      if (side !== null && o.color === side) continue
      const d = distM(o, w.lon, w.lat, w.alt)
      if (d < bd) { bd = d; bc = c }
    }
    w.scanT = now
    if (bc >= 0 && bd < w.bestD) { w.bestD = bd; w.bestSid = bc; w.bestT = now }
  }

  /** Who fired a weapon: its `Parent` when the recording names one,
   *  otherwise the nearest aircraft (or, failing that, ground unit or ship)
   *  within 1.5 km of where it first appeared. */
  private findLauncher(sid: number) {
    const w = this.objs[sid]
    if (w.parentTid !== null) {
      const p = this.alive.get(w.parentTid)
      if (p !== undefined && !isEffect(this.objs[p].kind)) { w.launcher = p; return }
    }
    let ba = Infinity, ca = -1, bo = Infinity, co = -1
    for (const c of this.targets) {
      if (c === sid) continue
      const o = this.objs[c]
      const d = distM(o, w.lon, w.lat, w.alt)
      if (d > 1500) continue
      if (isAir(o.kind)) { if (d < ba) { ba = d; ca = c } }
      else if (d < bo) { bo = d; co = c }
    }
    w.launcher = ca >= 0 ? ca : co
  }

  finish(opts: FinishOptions = {}): AcmiData {
    const end = this.now
    const objs = this.objs
    for (const o of objs) {
      if (!o.alive) continue
      o.t1 = end
      if (o.pending && o.positioned && !o.skip) o.emit()
    }
    let startMs = this.recordingTime ? Date.parse(this.recordingTime) : NaN
    if (Number.isNaN(startMs)) startMs = opts.fileEndMs != null ? opts.fileEndMs - end : 0

    // Meta indices, in order of first appearance.
    let next = 0
    for (const o of objs) if (!o.skip && o.kind !== null && o.n > 0) o.idx = next++
    const idxOf = (sid: number): number | null => (sid >= 0 && objs[sid].idx >= 0 ? objs[sid].idx : null)

    const events: RecEvent[] = []
    const shots = new Map<number, number>()
    const hits = new Map<number, number>()
    const kills = new Map<number, number>()
    const bump = (m: Map<number, number>, k: number) => m.set(k, (m.get(k) ?? 0) + 1)
    const landedLast = new Map<number, boolean>()
    // victim sid -> time of death, killing weapon sid (-1 unknown)
    const dead = new Map<number, { td: number; w: number }>()

    // Shots: who fired, at whom (the enemy it came closest to), how close. A
    // hit whose target leaves soon after is the kill -- within 90 s for an
    // aircraft (a wreck falls for a while), 15 s for anything else.
    const byVictim = new Map<number, { th: number; w: number }>()
    const weapons: number[] = []
    for (let wsid = 0; wsid < objs.length; wsid++) {
      const o = objs[wsid]
      const k = o.kind
      if (o.idx < 0 || k === null || !isWeapon(k)) continue
      weapons.push(wsid)
      const by = idxOf(o.launcher)
      if (by !== null) bump(shots, by)
      const hit = o.bestSid >= 0 && o.bestD <= lethalRadiusM(k)
      if (by !== null && hit) bump(hits, by)
      events.push({
        t: o.t0, k: 'fired', o: by, w: o.idx,
        tg: o.bestSid >= 0 ? idxOf(o.bestSid) : null,
        md: o.bestSid >= 0 ? Math.round(o.bestD) : null,
        hit, te: o.t1,
      })
      if (hit) {
        const vsid = o.bestSid, th = o.bestT
        const v = objs[vsid]
        const window = isAir(v.kind) ? 90_000 : 15_000
        if (v.removed && v.t1 >= th - 2000 && v.t1 <= th + window) {
          // The hit closest before the death takes the credit.
          const e = byVictim.get(vsid)
          if (!e) byVictim.set(vsid, { th, w: wsid })
          else if (th > e.th && th <= v.t1 + 2000) { e.th = th; e.w = wsid }
        }
      }
    }
    for (const [vsid, { w }] of byVictim) dead.set(vsid, { td: objs[vsid].t1, w })
    // Recordings that do carry Destroyed events: the weapon that ended
    // nearest the victim around then.
    for (const ev of this.evs) {
      if (ev.k !== 'destroyed' || dead.has(ev.sid)) continue
      const v = objs[ev.sid]
      const td = v.destroyedT >= 0 ? v.destroyedT : ev.t
      const [aLon, aLat, aAlt] = v.destroyedT >= 0 ? [v.dLon, v.dLat, v.dAlt] : [0, 0, 0]
      let bd = Infinity, bw = -1
      for (const wsid of weapons) {
        const w = objs[wsid]
        if (w.t1 < td - 5000 || w.t1 > td + 3000) continue
        const d = distM(w, aLon, aLat, aAlt)
        if (d < 300 && d < bd) { bd = d; bw = wsid }
      }
      dead.set(ev.sid, { td, w: bw })
    }
    for (const [vsid, { td, w }] of dead) {
      const o = idxOf(vsid)
      if (o === null) continue
      const wobj = w >= 0 ? objs[w] : null
      const by = wobj ? idxOf(wobj.launcher) : null
      if (by !== null) bump(kills, by)
      if (wobj) events.push({ t: td, k: 'kill', o, by, w: wobj.idx })
      else events.push({ t: td, k: 'destroyed', o })
    }
    // An aircraft that vanished in flight with nothing on it: shot down by
    // guns, crashed, collided, or its pilot left mid-air.
    for (let sid = 0; sid < objs.length; sid++) {
      const o = objs[sid]
      if (o.removedAirborne && !dead.has(sid) && o.idx >= 0) events.push({ t: o.t1, k: 'lost', o: o.idx })
    }
    const nav = this.evs.filter(e => e.k !== 'destroyed').sort((a, b) => a.t - b.t)
    for (const ev of nav) {
      const o = idxOf(ev.sid)
      if (o === null) continue
      const landing = ev.k === 'landing'
      landedLast.set(o, landing)
      events.push({ t: ev.t, k: landing ? 'landing' : 'takeoff', o })
    }
    for (const ev of this.tacEvs) events.push({ t: ev.t, k: ev.k, o: idxOf(ev.sid), text: ev.text })
    events.sort((a, b) => a.t - b.t)

    // meta index -> when it was destroyed or lost
    const deaths = new Map<number, number>()
    for (const [sid, { td }] of dead) { const i = idxOf(sid); if (i !== null) deaths.set(i, td) }
    for (const o of objs) if (o.removedAirborne && o.idx >= 0 && !deaths.has(o.idx)) deaths.set(o.idx, o.t1)

    const kept = objs.filter(o => o.idx >= 0)
    const objects: RecObject[] = kept.map(o => ({
      k: o.kind!, n: o.name, p: o.pilot, g: o.group, c: o.color, cs: o.callsign, co: o.country,
      t0: o.t0, t1: o.t1, par: idxOf(o.launcher), f: o.flags, d: deaths.get(o.idx) ?? null,
      ty: o.ty, id: idHex(o.id),
    }))

    const flights: RecFlight[] = []
    for (const o of kept) {
      if (!isAir(o.kind)) continue
      const pilot = o.pilot?.trim()
      if (!pilot) continue
      const i = o.idx
      flights.push({
        i, pilot, aircraft: o.name ?? '', kind: o.kind!, color: o.color, group: o.group,
        t0: o.t0, t1: o.t1, start_ms: startMs + o.t0, end_ms: startMs + o.t1,
        shots: shots.get(i) ?? 0, hits: hits.get(i) ?? 0, kills: kills.get(i) ?? 0,
        fate: deaths.has(i) ? 'destroyed' : landedLast.get(i) ? 'landed' : 'left',
      })
    }

    const meta: AcmiData['meta'] = {
      v: 4,
      title: this.title,
      author: this.author,
      data_source: this.dataSource,
      reference_time: this.referenceTime,
      recording_time: this.recordingTime,
      first_frame_s: this.firstFrameS,
      start_ms: startMs,
      duration_ms: end,
      chunk_ms: end || 1,
      chunks: 1,
      objects,
      events,
      flights,
    }
    return { meta, tracks: this.tracks(kept, objs) }
  }

  /** Copy the build columns into exact-length Float64 views over a few
   *  slabs, freeing each object's build columns as it goes. */
  private tracks(kept: Obj[], objs: Obj[]): Map<number, Seg> {
    let total = 0
    for (const o of kept) {
      let cols = 4
      if (o.croll) cols += 3
      if (o.cias) cols++
      if (o.cmach) cols++
      if (o.caoa) cols++
      if (o.cagl) cols++
      if (o.chans) cols += o.chans.size
      total += cols * o.n
    }
    const slabs = new Slabs(total)
    const out = new Map<number, Seg>()
    for (const o of kept) {
      const n = o.n
      const col = (a: Float64Array | Float32Array) => { const v = slabs.take(n); v.set(a.subarray(0, n)); return v }
      const seg: Seg = { t: col(o.ct), lon: col(o.clon), lat: col(o.clat), alt: col(o.calt) }
      if (o.croll) { seg.roll = col(o.croll); seg.pitch = col(o.cpitch!); seg.yaw = col(o.cyaw!) }
      if (o.cias) seg.ias = col(o.cias)
      if (o.cmach) seg.mach = col(o.cmach)
      if (o.caoa) seg.aoa = col(o.caoa)
      if (o.cagl) seg.agl = col(o.cagl)
      if (o.chans) {
        seg.ch = {}
        for (const [name, c] of o.chans) {
          const v = col(c.col)
          if (c.lock) for (let i = 0; i < n; i++) v[i] = v[i] >= 0 ? objs[v[i]].idx : -1
          seg.ch[name] = v
        }
      }
      o.dropColumns()
      out.set(o.idx, seg)
    }
    return out
  }
}

/** Build a recording held in one string (tests, small files; no worker). */
export function buildFromText(text: string, opts?: FinishOptions): AcmiData {
  const b = new Builder()
  const p = new AcmiParser(b)
  p.push(text)
  p.end()
  return b.finish(opts)
}
