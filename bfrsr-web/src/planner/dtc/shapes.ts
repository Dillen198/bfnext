// Lines and areas in a cartridge, drawn as lines rather than as boxes per
// point: F-16 geo lines (points tagged L1-L4), F-14 plot lines, Hornet
// FAOR/FLOT lines, Apache tactical lines, engagement areas and fire zones
// (shapes holding their own points). Pure logic for the cartridge map.

import { clone, getAt, toPointer, type Json, type JsonObject, type Seg } from '../model'
import { fromDcsXY, toDcsXY, type DcsTerrain } from '../nav'
import { humanise, isObj, rowLabel, type Ctx } from './core'

/** A list the map draws lines from. */
export interface ShapeList {
  segs: Seg[]
  /** "Plan 3 › Plot lines", "FLOT lines". */
  label: string
  /** shapes: each item is a line with its own points; flags: each item is a
   *  point, and its L1-L4 switch says which line it is on. */
  mode: 'shapes' | 'flags'
  /** shapes: the field holding the points ("points", "vertices"). */
  pointsKey: string
  /** flags: the line switches, "L1".."L4". */
  flags: string[]
  /** A new item: a shape (shapes) or a point (flags). */
  template: JsonObject
  pointTemplate: JsonObject
  maxItems: number | null
  minPts: number
  maxPts: number | null
  /** Exactly four corners: drawn as a rectangle from two clicks. */
  rect: boolean
  closed: 'always' | 'flag' | 'never'
  here: boolean
  editable: boolean
}

export interface ShapePoint { segs: Seg[]; lat: number; lon: number }

/** One line or area as drawn. `index` is the shape's index (shapes) or the
 *  line's switch index (flags). */
export interface Shape { list: ShapeList; index: number; name: string; closed: boolean; pts: ShapePoint[] }

const isPos = (v: Json | undefined): v is JsonObject => isObj(v) && typeof v.x === 'number' && typeof v.y === 'number'
const placed = (p: JsonObject) => p.x !== 0 || p.y !== 0
const flagKeys = (o: JsonObject) => Object.keys(o).filter(k => /^L\d$/.test(k) && typeof o[k] === 'boolean').sort()

/** Every list of lines or areas in the cartridge. */
export function shapeLists(ctx: Ctx, pageSegs: Seg[]): ShapeList[] {
  const out: ShapeList[] = []
  const walk = (v: Json | undefined, segs: Seg[], names: string[], managed: boolean, depth: number) => {
    if (depth > 8 || v === null || typeof v !== 'object') return
    const a = segs.length ? ctx.idx.get(segs) : undefined
    if (a?.hidden) return
    const m = managed || !!a?.managed
    if (Array.isArray(v)) {
      const tmpl = (a?.item ?? v[0] ?? null) as Json
      const list = isObj(tmpl) ? detect(ctx, segs, tmpl, v) : null
      if (list) {
        const label = [...names, a?.label ?? humanise(String(segs[segs.length - 1]))].join(' › ')
        const here = pageSegs.every((s, i) => String(segs[i]) === String(s))
        out.push({ ...list, segs, label, here, editable: here && !m && !ctx.readOnly })
        return
      }
      v.forEach((x, i) => walk(x, [...segs, i], [...names, rowLabel(ctx, segs, i, x)], m, depth + 1))
      return
    }
    for (const [k, x] of Object.entries(v)) walk(x, [...segs, k], names, m, depth + 1)
  }
  walk(ctx.value, [], [], false, 0)
  return out
}

type Detected = Omit<ShapeList, 'segs' | 'label' | 'here' | 'editable'>

function detect(ctx: Ctx, segs: Seg[], tmpl: JsonObject, v: Json[]): Detected | null {
  const a = ctx.idx.get(segs)
  const flags = flagKeys(tmpl)
  if (isPos(tmpl) && flags.length >= 2) {
    return { mode: 'flags', pointsKey: '', flags, template: tmpl, pointTemplate: tmpl, maxItems: a?.maxItems ?? null, minPts: 2, maxPts: null, rect: false, closed: 'never' }
  }
  // A shape: exactly one visible field that is a list of positions.
  const fields = Object.keys(tmpl).filter(k => {
    if (!Array.isArray(tmpl[k]) || ctx.idx.get([...segs, 0, k])?.hidden) return false
    const pa = ctx.idx.get([...segs, 0, k])
    const pt = (pa?.item ?? (tmpl[k] as Json[])[0] ?? v.map(x => (isObj(x) && Array.isArray(x[k]) ? (x[k] as Json[])[0] : undefined)).find(isPos)) as Json | undefined
    return isPos(pt)
  })
  if (fields.length !== 1) return null
  const k = fields[0]
  const pa = ctx.idx.get([...segs, 0, k])
  const pointTemplate = clone((pa?.item ?? (tmpl[k] as Json[])[0] ?? { x: 0, y: 0 }) as JsonObject)
  // Points that switch on and off are fixed slots (F-14 JDAM targets), not a line.
  if (typeof pointTemplate.active === 'boolean') return null
  const fixed = pa?.fixedLength ?? null
  const rect = fixed === 4
  return {
    mode: 'shapes', pointsKey: k, flags: [], template: tmpl, pointTemplate,
    maxItems: a?.maxItems ?? null,
    minPts: fixed ?? pa?.minItems ?? 2,
    maxPts: fixed ?? pa?.maxItems ?? null,
    rect,
    closed: rect ? 'always' : typeof tmpl.closed === 'boolean' ? 'flag' : 'never',
  }
}

/** The lines and areas of a list, ready to draw. */
export function shapesOf(ctx: Ctx, map: DcsTerrain, list: ShapeList): Shape[] {
  const v = (getAt(ctx.value, list.segs) as Json[] | undefined) ?? []
  const at = (p: JsonObject, segs: Seg[]): ShapePoint => {
    const ll = typeof p.lat === 'number' && typeof p.lon === 'number' && (p.lat !== 0 || p.lon !== 0)
      ? { lat: p.lat, lon: p.lon }
      : fromDcsXY(p.x as number, p.y as number, map.terrain)
    return { segs, ...ll }
  }
  if (list.mode === 'flags') {
    return list.flags.map((f, index) => ({
      list, index, name: `Line ${index + 1}`, closed: false,
      pts: v.flatMap((p, i) => (isPos(p) && p[f] === true && placed(p) ? [at(p, [...list.segs, i])] : [])),
    })).filter(s => s.pts.length)
  }
  return v.flatMap((item, index) => {
    if (!isObj(item)) return []
    const raw = (item[list.pointsKey] as Json[] | undefined) ?? []
    const pts = raw.flatMap((p, j) => (isPos(p) && placed(p) ? [at(p, [...list.segs, index, list.pointsKey, j])] : []))
    const closed = list.closed === 'always' || (list.closed === 'flag' && item.closed === true)
    return [{ list, index, name: shapeName(ctx, list, index, item), closed, pts }]
  })
}

/** What the cockpit calls a shape: an Apache line by its type ("FLOT",
 *  "PL ABC"), an area by its number ("EA3"), else the list's own label. */
function shapeName(ctx: Ctx, list: ShapeList, index: number, item: JsonObject): string {
  const type = ctx.idx.get([...list.segs, index, 'type_num'])?.options?.find(o => o.value === item.type_num)
  if (type) {
    const abbr = String(type.label).split(' (')[0]
    return typeof item.text === 'string' && item.text.trim() ? `${abbr} ${item.text.trim()}` : abbr
  }
  const base = rowLabel(ctx, list.segs, index, item)
  const prefix = /\(([A-Z]+)1-[A-Z]*\d+\)/.exec(list.label)
  return prefix && /^#?\d+$/.test(base) ? `${prefix[1]}${index + 1}` : base
}

/** Pointers of every list point the shapes draw, so the map does not box them too. */
export function shapePointers(lists: ShapeList[]): string[] {
  return lists.map(l => toPointer(l.segs))
}

/** A point with its map position filled in (x/y, and lat/lon where the
 *  cartridge keeps them). */
function at(tmpl: JsonObject, lat: number, lon: number, map: DcsTerrain): JsonObject {
  const p = clone(tmpl)
  const { x, y } = toDcsXY(lat, lon, map.terrain)
  p.x = x
  p.y = y
  if ('lat' in p) p.lat = lat
  if ('lon' in p) p.lon = lon
  return p
}

/** The list after adding a point: to shape `shape` (shapes), as a new shape
 *  when `shape` is null, or to line `flag` (flags). Null when it is full. */
export function addPoint(ctx: Ctx, list: ShapeList, target: { shape: number | null; flag: number }, lat: number, lon: number, map: DcsTerrain): Json[] | null {
  const v = clone((getAt(ctx.value, list.segs) as Json[] | undefined) ?? [])
  if (list.mode === 'flags') {
    if (list.maxItems != null && v.length >= list.maxItems) return null
    const p = at(list.pointTemplate, lat, lon, map)
    list.flags.forEach((f, i) => { p[f] = i === target.flag })
    v.push(p)
    return renumber(v, list)
  }
  if (target.shape == null || !isObj(v[target.shape])) {
    if (list.maxItems != null && v.length >= list.maxItems) return null
    const s = clone(list.template)
    s[list.pointsKey] = [at(list.pointTemplate, lat, lon, map)]
    v.push(s)
    return renumber(v, list)
  }
  const s = v[target.shape] as JsonObject
  const pts = ((s[list.pointsKey] as Json[] | undefined) ?? []).filter(p => isPos(p) && placed(p))
  if (list.maxPts != null && pts.length >= list.maxPts) return null
  s[list.pointsKey] = [...pts, at(list.pointTemplate, lat, lon, map)]
  return renumber(v, list)
}

/** The list with a new rectangle (Apache areas and zones: 4 corners square
 *  to the map grid, the way the Mission Editor draws them). */
export function addRect(ctx: Ctx, list: ShapeList, a: { lat: number; lon: number }, b: { lat: number; lon: number }, map: DcsTerrain): Json[] | null {
  const v = clone((getAt(ctx.value, list.segs) as Json[] | undefined) ?? [])
  if (list.maxItems != null && v.length >= list.maxItems) return null
  const p = toDcsXY(a.lat, a.lon, map.terrain)
  const q = toDcsXY(b.lat, b.lon, map.terrain)
  const corner = (x: number, y: number) => ({ ...clone(list.pointTemplate), x, y })
  const s = clone(list.template)
  s[list.pointsKey] = [corner(p.x, p.y), corner(p.x, q.y), corner(q.x, q.y), corner(q.x, p.y)]
  v.push(s)
  return renumber(v, list)
}

/** The list without the point at `segs`; a shape left with no points goes too. */
export function removePoint(ctx: Ctx, list: ShapeList, segs: Seg[]): Json[] {
  const v = clone((getAt(ctx.value, list.segs) as Json[] | undefined) ?? [])
  const rel = segs.slice(list.segs.length)
  if (list.mode === 'flags') {
    v.splice(rel[0] as number, 1)
    return renumber(v, list)
  }
  const s = v[rel[0] as number] as JsonObject
  const pts = (s[list.pointsKey] as Json[]).filter((_, j) => j !== rel[2])
  // An area keeps its four corners: deleting one deletes the area.
  if (pts.length === 0 || list.rect) v.splice(rel[0] as number, 1)
  else s[list.pointsKey] = pts
  return renumber(v, list)
}

/** Numbers and ids that follow the array index (num, "FLOT_2", "FLOT_2_PT_3",
 *  "GEO_LINES32"), recomputed after any change. */
export function renumber(v: Json[], list: ShapeList): Json[] {
  const tmpl = list.mode === 'flags' ? list.pointTemplate : list.template
  const numField = ['num', 'number', 'Number'].find(f => typeof tmpl[f] === 'number')
  const idBase = typeof tmpl.id === 'string' && numField ? /^(.*?)(\d+)$/.exec(tmpl.id) : null
  v.forEach((x, i) => {
    if (!isObj(x)) return
    const n = i + 1
    if (numField) x[numField] = n
    if (idBase && numField) x.id = `${idBase[1]}${Number(idBase[2]) - (tmpl[numField] as number) + n}`
    if (list.mode === 'shapes' && Array.isArray(x[list.pointsKey])) {
      ;(x[list.pointsKey] as Json[]).forEach((p, j) => {
        if (!isObj(p) || typeof p.id !== 'string') return
        const m = /^(.*?)(\d+)(_PT_)(\d+)$/.exec(p.id)
        if (m) p.id = `${m[1]}${n}${m[3]}${j + 1}`
      })
    }
  })
  return v
}
