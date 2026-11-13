/**
 * A debrief clip for a mock record, shaped like bfdb's `clip.rs` output, so
 * the 3D debrief has something to play with no server. Built from the
 * record's own track: the pilot's aircraft (with an attitude worked out from
 * its path, since the track has none), the boat for a carrier pass, the bomb
 * and its target for a drop, the adversary for a fight.
 */
import { deckFor } from '../lib/carrierDeck'
import { FT, KT, bearingDeg, destination, distanceM, norm180 } from '../lib/geo'
import type { Clip, ClipObj, GeoPt, RangeRecord } from '../types'
import { CARRIERS } from './world'

interface P { t: number; lat: number; lon: number; alt: number }

const G = 9.81

/** Heading along the path, climb angle, and the bank a coordinated turn at that rate needs. */
function attitude(ps: P[], flyer: boolean): { roll: number[]; pitch: number[]; yaw: number[] } {
  const n = ps.length
  const yaw: number[] = [], pitch: number[] = [], roll: number[] = []
  for (let i = 0; i < n; i++) {
    const a = ps[Math.max(0, i - 1)], b = ps[Math.min(n - 1, i + 1)]
    const d = distanceM(a, b)
    yaw.push(d > 0.5 ? bearingDeg(a, b) : yaw[i - 1] ?? 0)
    pitch.push(d > 0.5 ? (Math.atan2(b.alt - a.alt, d) * 180) / Math.PI : 0)
  }
  for (let i = 0; i < n; i++) {
    if (!flyer) { roll.push(0); continue }
    const a = Math.max(0, i - 2), b = Math.min(n - 1, i + 2)
    const dt = ps[b].t - ps[a].t
    const dpsi = (norm180(yaw[b] - yaw[a]) * Math.PI) / 180
    const v = dt > 0 ? distanceM(ps[a], ps[b]) / dt : 0
    const w = dt > 0 ? dpsi / dt : 0
    roll.push(Math.max(-80, Math.min(80, (Math.atan2(v * w, G) * 180) / Math.PI)))
  }
  return { roll, pitch, yaw }
}

function obj(id: string, kind: string, role: ClipObj['role'], ps: P[], extra: Partial<ClipObj> = {}, flyer = true): ClipObj {
  const att = attitude(ps, flyer)
  return {
    id, kind, role, t: ps.map(p => p.t), lat: ps.map(p => p.lat), lon: ps.map(p => p.lon), alt: ps.map(p => p.alt),
    roll: att.roll, pitch: att.pitch, yaw: att.yaw, destroyed: false, ...extra,
  }
}

const fromTrack = (pts: { t: number; lat: number; lon: number; alt_m: number }[], t0: number): P[] =>
  pts.map(p => ({ t: p.t - t0, lat: p.lat, lon: p.lon, alt: p.alt_m }))

/** A straight leg flown at `kts` that ends at `at` on `hdg`, `secs` long, sampled every half second. */
function legInto(at: GeoPt, hdg: number, kts: number, secs: number, tEnd: number, climbDeg = 0): P[] {
  const out: P[] = []
  for (let s = secs; s >= 0; s -= 0.5) {
    const d = s * kts * KT
    const p = destination(at, hdg + 180, d)
    out.push({ t: tEnd - s, lat: p.lat, lon: p.lon, alt: at.alt_m + Math.tan((climbDeg * Math.PI) / 180) * d })
  }
  return out
}

/** A level turn away after the release / pass: `secs` long at `kts`, `deg` of turn. */
function egress(from: P, hdg: number, kts: number, secs: number, deg: number, climb: number): P[] {
  const out: P[] = []
  let p: GeoPt = { lat: from.lat, lon: from.lon, alt_m: from.alt }
  for (let s = 0.5; s <= secs; s += 0.5) {
    const h = hdg + (deg * s) / secs
    p = { ...destination(p, h, kts * KT * 0.5), alt_m: p.alt_m + climb * 0.5 }
    out.push({ t: from.t + s, lat: p.lat, lon: p.lon, alt: p.alt_m })
  }
  return out
}

export function mockClip(r: RangeRecord): Clip | null {
  const t = r.track
  const res = r.result
  const base = { v: 1, reference_time: '2024-08-01T00:00:00Z', complete: true, source: 'mock.zip.acmi' }
  const self = { name: r.unit_type, pilot: r.pilot.name, color: 'Blue', coalition: 'Enemies' }

  if (res.kind === 'trap' && t?.kind === 'groove' && t.samples.length > 1) {
    const cv = CARRIERS.find(c => c.unit_type === res.carrier_type) ?? CARRIERS[0]
    const geom = deckFor(cv.unit_type)
    const tEnd = t.samples[t.samples.length - 1].t
    const me = t.samples.map(s => ({ t: s.t - tEnd, lat: s.lat, lon: s.lon, alt: s.alt_ft * FT + geom.deck_height_m }))
    // the mock pass is flown against a ship holding still; the clip says so
    const boat = obj('b0', 'carrier', 'boat', [
      { t: me[0].t, lat: cv.pos.lat, lon: cv.pos.lon, alt: 0 },
      { t: 0, lat: cv.pos.lat, lon: cv.pos.lon, alt: 0 },
    ], { name: cv.unit_type, color: 'Blue' }, false)
    boat.yaw = boat.yaw.map(() => cv.brc_deg)
    // the plane guard holds station on the starboard quarter
    const guard = destination(cv.pos, cv.brc_deg + 135, 1800)
    const helo = obj('c1', 'helo', 'air', [
      { t: me[0].t, lat: guard.lat, lon: guard.lon, alt: 90 },
      { t: 0, lat: guard.lat, lon: guard.lon, alt: 90 },
    ], { name: 'SH-60B', pilot: 'Plane guard', color: 'Blue' }, false)
    helo.yaw = helo.yaw.map(() => cv.brc_deg)
    return { ...base, event_s: 3600, t0: me[0].t, t1: 0, self_id: 'a0', objects: [obj('a0', 'air', 'self', me, self), boat, helo] }
  }

  if (res.kind === 'bomb' && t?.kind === 'weapon' && t.points.length > 1) {
    const tEnd = t.points[t.points.length - 1].t
    const bomb = fromTrack(t.points, tEnd)
    const rel = res.release
    const relT = bomb[0].t
    const run = legInto({ ...rel.pos, alt_m: bomb[0].alt }, rel.heading_deg, rel.tas_kts, 35, relT, rel.dive_deg)
    const off = egress(run[run.length - 1], rel.heading_deg, rel.tas_kts, Math.max(12, -relT + 10), 120, 20)
    const target = obj('d0', 'armor', 'ground', [
      { t: run[0].t, lat: res.target_pos.lat, lon: res.target_pos.lon, alt: res.target_pos.alt_m },
      { t: off[off.length - 1].t, lat: res.target_pos.lat, lon: res.target_pos.lon, alt: res.target_pos.alt_m },
    ], { name: res.target || 'Target', color: 'Red', destroyed: res.target_hit }, false)
    return {
      ...base, event_s: 3600, t0: run[0].t, t1: off[off.length - 1].t, self_id: 'a0',
      objects: [
        obj('a0', 'air', 'self', [...run, ...off], self),
        obj('w0', 'bomb', 'weapon', bomb, { name: res.weapon, parent: 'a0', gone: 0 }, false),
        target,
      ],
    }
  }

  if (t?.kind === 'path') {
    const names = Object.keys(t.paths)
    const all = names.flatMap(n => t.paths[n])
    if (!all.length) return null
    const tEnd = Math.max(...all.map(p => p.t))
    const objects = names.map((n, i) => obj(
      i === 0 ? 'a0' : `a${i}`, /^(UH|CH|AH|Mi|Ka|SA342|OH)/.test(r.unit_type) && n === r.pilot.name ? 'helo' : 'air',
      n === r.pilot.name ? 'self' : 'air', fromTrack(t.paths[n], tEnd),
      n === r.pilot.name ? self : { name: 'Su-27', pilot: n, color: 'Red' },
    ))
    objects.sort((a, b) => (a.role === 'self' ? -1 : b.role === 'self' ? 1 : 0))
    const t0 = Math.min(...objects.map(o => o.t[0]))
    return { ...base, event_s: 3600, t0, t1: 0, self_id: objects[0].id, objects }
  }

  if (t?.kind === 'intercept' && t.target.length > 1) {
    const tEnd = Math.max(t.target[t.target.length - 1].t, t.missile[t.missile.length - 1]?.t ?? 0)
    const me = fromTrack(t.target, tEnd)
    return {
      ...base, event_s: 3600, t0: me[0].t, t1: 0, self_id: 'a0',
      objects: [obj('a0', 'air', 'self', me, self), obj('m0', 'missile', 'threat', fromTrack(t.missile, tEnd), { name: 'R-27ER' }, false)],
    }
  }

  if (t?.kind === 'weapon' && t.points.length > 1) {
    // a weapon with no launcher path: put the shooter on the weapon's first heading
    const tEnd = t.points[t.points.length - 1].t
    const w = fromTrack(t.points, tEnd)
    const hdg = bearingDeg(w[0], w[1])
    const at: GeoPt = { lat: w[0].lat, lon: w[0].lon, alt_m: w[0].alt }
    const run = legInto(at, hdg, 420, 30, w[0].t)
    const off = egress(run[run.length - 1], hdg, 420, Math.max(10, -w[0].t + 5), 100, 15)
    return {
      ...base, event_s: 3600, t0: run[0].t, t1: off[off.length - 1].t, self_id: 'a0',
      objects: [obj('a0', 'air', 'self', [...run, ...off], self), obj('w0', 'missile', 'weapon', w, { name: 'weapon', parent: 'a0', gone: 0 }, false)],
    }
  }
  return null
}
