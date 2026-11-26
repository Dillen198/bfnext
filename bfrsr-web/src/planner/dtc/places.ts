// Where cartridge items sit on the map, and how the map edits them: pure
// logic shared by the cartridge map and its tests.

import { clone, getAt, patternSegs, toPointer, type Json, type JsonObject, type Seg } from '../model'
import { fromDcsXY, type DcsTerrain } from '../nav'
import { isObj, rowLabel, type Ctx } from './core'
import { shapeLists } from './shapes'

/** A cartridge item with a map position. */
export interface Pos {
  segs: Seg[]
  lat: number
  lon: number
  label: string
  /** On the page being edited (bright, draggable) or elsewhere (faint). */
  here: boolean
  editable: boolean
}

const under = (segs: Seg[], root: Seg[]) => root.every((s, i) => String(segs[i]) === String(s))

/** Every object with a DCS map position (x north, y east) in the cartridge. */
export function positions(ctx: Ctx, map: DcsTerrain, pageSegs: Seg[]): Pos[] {
  const out: Pos[] = []
  // Points of lines and areas are drawn as lines, not each in a box.
  const lines = new Set(shapeLists(ctx, pageSegs).map(l => toPointer(l.segs)))
  const walk = (v: Json | undefined, segs: Seg[], managed: boolean, depth: number) => {
    if (out.length > 600 || depth > 9 || v === null || typeof v !== 'object' || lines.has(toPointer(segs))) return
    const a = segs.length ? ctx.idx.get(segs) : undefined
    const m = managed || !!a?.managed
    if (isObj(v) && typeof v.x === 'number' && typeof v.y === 'number' && (v.x !== 0 || v.y !== 0) && v.active !== false) {
      const ll = fromDcsXY(v.x, v.y, map.terrain)
      const parent = segs.slice(0, -1)
      out.push({
        segs, lat: ll.lat, lon: ll.lon,
        label: rowLabel(ctx, parent, segs[segs.length - 1], v),
        here: under(segs, pageSegs), editable: !m && !ctx.readOnly,
      })
    }
    const entries: [Seg, Json][] = Array.isArray(v) ? v.map((x, i) => [i, x]) : Object.entries(v)
    for (const [k, x] of entries) walk(x, [...segs, k], m, depth + 1)
  }
  walk(ctx.value, [], false, 0)
  return out
}

export interface AddableList { segs: Seg[]; template: JsonObject; label: string }

/** Every editable list of positioned items on the page, where a map click
 *  adds one ("Plan 3 › Additional points"). Line points are not here: the
 *  map draws those with shapes.ts. */
export function addableLists(ctx: Ctx, pageSegs: Seg[]): AddableList[] {
  if (ctx.readOnly) return []
  const lines = new Set(shapeLists(ctx, pageSegs).map(l => toPointer(l.segs)))
  const out: AddableList[] = []
  const queue: [Seg[], string[]][] = [[pageSegs, []]]
  while (queue.length) {
    const [segs, names] = queue.shift()!
    const v = getAt(ctx.value, segs)
    const a = ctx.idx.get(segs)
    if (a?.managed || a?.hidden || lines.has(toPointer(segs))) continue
    if (Array.isArray(v)) {
      const tmpl = (a?.item ?? v[v.length - 1] ?? null) as Json
      const full = a?.maxItems != null && v.length >= a.maxItems
      if (isObj(tmpl) && typeof tmpl.x === 'number' && typeof tmpl.y === 'number') {
        if (!full && a?.fixedLength == null) out.push({ segs, template: tmpl, label: [...names, a?.label ?? 'point'].join(' › ') })
        continue
      }
      v.forEach((x, i) => { if (isObj(x)) queue.push([[...segs, i], [...names, rowLabel(ctx, segs, i, x)]]) })
      continue
    }
    if (isObj(v)) for (const k of Object.keys(v)) if (v[k] !== null && typeof v[k] === 'object') queue.push([[...segs, k], names])
  }
  return out
}

/** The first of them: where a plain map click adds a point. */
export function addableList(ctx: Ctx, pageSegs: Seg[]): AddableList | null {
  return addableLists(ctx, pageSegs)[0] ?? null
}

/** A fixed slot that holds a position once switched on (F-14 JDAM targets). */
export interface Slot { segs: Seg[]; label: string; placed: boolean }

/** Lists of switchable slots on the page: arrays whose items all carry a
 *  boolean `active`. Labelled by their path, e.g. "STA 3 › PP1". */
export function slotsOn(ctx: Ctx, pageSegs: Seg[]): Slot[] {
  const out: Slot[] = []
  const walk = (segs: Seg[], names: string[], depth: number) => {
    const v = getAt(ctx.value, segs)
    if (depth > 6 || v === null || typeof v !== 'object' || ctx.idx.get(segs)?.hidden) return
    if (Array.isArray(v)) {
      const slots = v.length > 0 && v.every(x => isObj(x) && typeof x.active === 'boolean')
      v.forEach((x, i) => {
        const name = rowLabel(ctx, segs, i, x)
        if (slots) {
          const o = x as JsonObject
          out.push({ segs: [...segs, i], label: [...names, name].join(' › '), placed: o.active === true && typeof o.x === 'number' })
        } else walk([...segs, i], [...names, name], depth + 1)
      })
      return
    }
    for (const k of Object.keys(v)) walk([...segs, k], names, depth + 1)
  }
  walk(pageSegs, [], 0)
  return out
}

/** Whether a page has anything to show on a map: route-filled nav, a list
 *  of points to add to, or items that carry a map position. Countermeasures,
 *  radios, RWR tables and the like do not. */
export function pageHasMap(ctx: Ctx, segs: Seg[], routeFilled: boolean): boolean {
  if (routeFilled || addableList(ctx, segs) || slotsOn(ctx, segs).length || shapeLists(ctx, segs).some(l => l.here)) return true
  // The route fills something deeper in the page (the F-14's flight plan 2).
  const page = segs.map(s => (typeof s === 'number' ? '[]' : String(s)))
  if (ctx.idx.ann.fields.some(f => f.managed === 'route' && patternSegs(f.path).length > page.length && page.every((p, i) => {
    const q = patternSegs(f.path)[i]
    return q === p || q === '[]' || q === '{}'
  }))) return true
  const walk = (v: Json | undefined, depth: number): boolean => {
    if (depth > 8 || v === null || typeof v !== 'object') return false
    if (isObj(v) && typeof v.x === 'number' && typeof v.y === 'number') return true
    return (Array.isArray(v) ? v : Object.values(v)).some(x => walk(x, depth + 1))
  }
  return walk(getAt(ctx.value, segs), 0)
}

/** Number a new list item on from the last: num/number fields, and an id like
 *  "DEST81" whose numeric part keeps the template's offset from its number. */
export function numbered(list: Json[], tmpl: JsonObject): JsonObject {
  const item = clone(tmpl)
  const last = (list[list.length - 1] ?? null) as JsonObject | null
  for (const f of ['num', 'number', 'Number', 'wypt_num']) {
    if (typeof item[f] === 'number') {
      const prev = last && typeof last[f] === 'number' ? (last[f] as number) : (tmpl[f] as number) - 1
      const n = prev + 1
      if (typeof item.id === 'string') {
        const m = /^(.*?)(\d+)$/.exec(item.id)
        if (m) item.id = `${m[1]}${Number(m[2]) - (tmpl[f] as number) + n}`
      }
      item[f] = n
    }
  }
  return item
}

