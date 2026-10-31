// Things on the cartridge map a pilot can pick and send into the cartridge:
// what JTACs, AWACS and recon have seen, intel threat rings, objectives,
// tasking, the bullseye. Plain logic; the map layers are IntelLayers.tsx.

import * as mgrs from 'mgrs'
import type { AirTrack, GroundContact, MapObjective, SituationReport, TacPicture, Task, ThreatArea } from '../../api'
import { clone, getAt, type Json, type JsonObject, type Seg } from '../model'
import { toDcsXY, type DcsTerrain } from '../nav'
import { ddm, dms } from '../dcsdtc'
import { MAX_WAYPOINTS, type Plan, type PlanWaypoint } from '../types'
import type { Ctx } from './core'
import { addableLists, numbered, slotsOn } from './places'
import { shortName } from './mission'

export type PickKind = 'contact' | 'threat' | 'objective' | 'task' | 'air' | 'bullseye'

/** The map's switchable layers. */
export type LayerKey = 'front' | 'objective' | 'threat' | 'task' | 'contact' | 'air' | 'bullseye'
export const LAYERS: { key: LayerKey; label: string }[] = [
  { key: 'contact', label: 'JTAC / AWACS / recon contacts' },
  { key: 'threat', label: 'Intel threat rings' },
  { key: 'task', label: 'Tasking' },
  { key: 'objective', label: 'Objectives' },
  { key: 'front', label: 'Frontline' },
  { key: 'bullseye', label: 'Bullseye' },
  { key: 'air', label: 'Enemy air' },
]

export interface Pick {
  /** Stable across refreshes of the same thing. */
  key: string
  kind: PickKind
  name: string
  /** One line: what it is, who saw it, how old. */
  detail: string
  lat: number
  lon: number
  /** A threat ring to carry over (SAM range, intel ring). */
  radius_m?: number
  /** How far off the position may be. */
  uncertainty_m?: number
  /** Worth a target point (enemy things), not just a reference. */
  hostile: boolean
}

export interface Intel {
  report: SituationReport | null
  tac: TacPicture | null
}

const CLASS_NAME: Record<GroundContact['class'], string> = {
  armor: 'Armour', airdefense: 'Air defence', artillery: 'Artillery', infantry: 'Infantry', airbase: 'Airbase', naval: 'Naval', unknown: 'Ground contact',
}
const SOURCE_NAME: Record<GroundContact['source'], string> = {
  recon: 'recon flight', sf: 'special forces', awacs: 'AWACS', ewr: 'EWR', jtac: 'JTAC', humint: 'HUMINT',
}

export function age(s: number): string {
  if (s < 90) return `${Math.round(s)} s ago`
  if (s < 5400) return `${Math.round(s / 60)} min ago`
  return `${Math.round(s / 3600)} h ago`
}

export const contactPick = (c: GroundContact): Pick => ({
  key: `contact-${c.id}`, kind: 'contact',
  name: `${c.count > 1 ? `${c.count}x ` : ''}${CLASS_NAME[c.class]}`,
  detail: `Seen by ${SOURCE_NAME[c.source]}, ${age(c.age_s)}, ±${Math.round(c.uncertainty_m)} m, ${Math.round(c.confidence * 100)}% sure`,
  lat: c.lat, lon: c.lon, radius_m: c.threat_range_m ?? undefined, uncertainty_m: c.uncertainty_m, hostile: true,
})

export const threatPick = (t: ThreatArea, i: number): Pick => ({
  key: `threat-${i}-${t.lat.toFixed(3)}-${t.lon.toFixed(3)}`, kind: 'threat', name: t.label,
  detail: `Intel (${t.source}), ${age(t.age_s)}${t.near ? `, near ${t.near}` : ''}${t.radius_m ? `, ring ${(t.radius_m / 1852).toFixed(1)} NM` : ''}`,
  lat: t.lat, lon: t.lon, radius_m: t.radius_m, uncertainty_m: t.uncertainty_m, hostile: true,
})

export const objectivePick = (o: MapObjective, side: string): Pick => ({
  key: `obj-${o.name}`, kind: 'objective', name: o.name,
  detail: `${o.kind}, ${o.owner === side ? 'yours' : o.owner === 'Neutral' ? 'neutral' : 'enemy'}, health ${o.health}%`,
  lat: o.lat, lon: o.lon, hostile: o.owner !== side && o.owner !== 'Neutral',
})

export const taskPick = (t: Task, n: number): Pick => ({
  key: `task-${t.id}`, kind: 'task', name: `Task ${n}: ${t.title}`, detail: t.detail,
  lat: t.lat, lon: t.lon, hostile: true,
})

export const airPick = (a: AirTrack): Pick => ({
  key: `air-${a.id}`, kind: 'air', name: a.unit_type ?? `${a.class} track`,
  detail: `${Math.round(a.alt_m * 3.28084 / 100) * 100} ft, ${Math.round(a.speed_kts)} kt, heading ${Math.round(a.heading)}°, ${age(a.age_s)}`,
  lat: a.lat, lon: a.lon, hostile: a.iff === 'hostile',
})

/** Everything pickable, for area selection. */
export function allPicks(intel: Intel, side: string, show: (k: PickKind) => boolean): Pick[] {
  const r = intel.report
  const out: Pick[] = []
  if (show('contact')) out.push(...(intel.tac?.ground ?? []).filter(c => c.side !== side).map(contactPick))
  if (show('threat')) out.push(...(r?.threats ?? []).filter(t => t.lat || t.lon).map(threatPick))
  if (show('task')) out.push(...(r?.tasking ?? []).filter(t => t.lat || t.lon).map((t, i) => taskPick(t, i + 1)))
  if (show('objective')) out.push(...(r?.map ?? []).filter(o => o.lat || o.lon).map(o => objectivePick(o, side)))
  if (show('air')) out.push(...(intel.tac?.air ?? []).filter(a => a.iff === 'hostile').map(airPick))
  return out
}

/** The ways a pilot reads coordinates, for copying into anything. */
export function coordFormats(lat: number, lon: number, map: DcsTerrain | null): { label: string; value: string }[] {
  const out = [
    { label: 'Deg min', value: `${ddm(lat, true, 3)} ${ddm(lon, false, 3)}` },
    { label: 'Deg min sec', value: `${dms(lat, true)} ${dms(lon, false)}` },
    { label: 'Decimal', value: `${lat.toFixed(5)}, ${lon.toFixed(5)}` },
  ]
  try { out.push({ label: 'MGRS', value: mgrs.forward([lon, lat], 5).replace(/^(\d+[A-Z])([A-Z]{2})(\d{5})(\d{5})$/, '$1 $2 $3 $4') }) } catch { /* outside MGRS */ }
  if (map) {
    const { x, y } = toDcsXY(lat, lon, map.terrain)
    out.push({ label: 'DCS x / y', value: `${Math.round(x)} / ${Math.round(y)}` })
  }
  return out
}

/** Where picks can go in this plan and cartridge. */
export interface SendTarget {
  key: string
  label: string
  kind: 'route' | 'list' | 'slots'
  segs?: Seg[]
  template?: JsonObject
  /** Slots: each slot's path, free ones first. */
  slots?: { segs: Seg[]; placed: boolean }[]
  room: number
}

export function sendTargets(ctx: Ctx, plan: Plan, pageSegs: Seg[]): SendTarget[] {
  const route: SendTarget = { key: 'route', label: 'Route, as target waypoints', kind: 'route', room: MAX_WAYPOINTS - plan.waypoints.length }
  if (ctx.readOnly) return [route]
  const under = (segs: Seg[]) => pageSegs.length > 0 && pageSegs.every((s, i) => String(segs[i]) === String(s))
  const out: SendTarget[] = []
  for (const l of addableLists(ctx, [])) {
    const a = ctx.idx.get(l.segs)
    const n = ((getAt(ctx.value, l.segs) as Json[] | undefined) ?? []).length
    out.push({ key: l.segs.join('/'), label: l.label, kind: 'list', segs: l.segs, template: l.template, room: a?.maxItems != null ? a.maxItems - n : 99 })
  }
  // Fixed slots (F-14 JDAM targets), one choice per station.
  const groups: Record<string, SendTarget> = {}
  for (const s of slotsOn(ctx, [])) {
    const name = s.label.split(' › ').slice(0, -1).join(' › ') || 'Target slots'
    const g = groups[name] ??= { key: `slots:${name}`, label: `${name}: target slots`, kind: 'slots', segs: s.segs.slice(0, -1), slots: [], room: 0 }
    g.slots!.push({ segs: s.segs, placed: s.placed })
    if (!s.placed) g.room++
  }
  out.push(...Object.values(groups))
  // What the page being edited holds comes first, then the route, then the rest.
  const here = out.filter(t => t.segs && under(t.segs))
  return [...here, route, ...out.filter(t => !here.includes(t))]
}

/** What sending these picks there changes. */
export interface Sent { waypoints?: PlanWaypoint[]; writes?: [Seg[], Json][]; added: number; skipped: number }

export function sendTo(ctx: Ctx, plan: Plan, to: SendTarget, picks: Pick[], map: DcsTerrain): Sent {
  if (to.kind === 'route') {
    const room = Math.max(0, MAX_WAYPOINTS - plan.waypoints.length)
    const add = picks.slice(0, room)
    const last = plan.waypoints[plan.waypoints.length - 1]
    const w = add.map((p): PlanWaypoint => ({
      name: p.name.toUpperCase().replace(/[^A-Z0-9 -]/g, '').slice(0, 24),
      lat: p.lat, lon: p.lon, alt_ft: last?.alt_ft ?? 20000, elev_ft: 0, target: p.hostile, note: p.detail.slice(0, 120),
    }))
    return { waypoints: [...plan.waypoints, ...w], added: add.length, skipped: picks.length - add.length }
  }
  if (to.kind === 'slots') {
    const free = (to.slots ?? []).filter(s => !s.placed)
    const writes: [Seg[], Json][] = picks.slice(0, free.length).map((p, i) => {
      const cur = getAt(ctx.value, free[i].segs) as JsonObject
      const { x, y } = toDcsXY(p.lat, p.lon, map.terrain)
      return [free[i].segs, { ...cur, active: true, x, y, lat: p.lat, lon: p.lon }]
    })
    return { writes, added: writes.length, skipped: picks.length - writes.length }
  }
  const segs = to.segs!
  const list = clone((getAt(ctx.value, segs) as Json[] | undefined) ?? []) as JsonObject[]
  const max = ctx.idx.get(segs)?.maxItems ?? Infinity
  let added = 0
  for (const p of picks) {
    if (list.length >= max) break
    const item = numbered(list, to.template!)
    const { x, y } = toDcsXY(p.lat, p.lon, map.terrain)
    item.x = x
    item.y = y
    if ('lat' in item) item.lat = p.lat
    if ('lon' in item) item.lon = p.lon
    // Its name where the list has one, cut to what the jet shows.
    const room = (f: string) => ctx.idx.get([...segs, list.length, f])?.maxLength
    if (typeof item.text === 'string') item.text = shortName(p.name, room('text') ?? 3)
    if (typeof item.note === 'string') item.note = p.name.slice(0, room('note') ?? 30)
    if (typeof item.name === 'string') item.name = p.name.slice(0, room('name') ?? 24)
    // Threat lists keep the ring the intel gave it.
    if (p.radius_m && typeof item.radius === 'number' && ('def_num' in item ? item.def_num === 1 : true)) item.radius = Math.round(p.radius_m)
    if (p.radius_m && typeof item.threat_ring_radius === 'number') item.threat_ring_radius = Math.min(500, Math.max(1, Math.round((p.radius_m / 1852) * 10) / 10))
    list.push(item)
    added++
  }
  return { writes: added ? [[segs, list]] : [], added, skipped: picks.length - added }
}
