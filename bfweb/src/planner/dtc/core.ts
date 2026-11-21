// The cartridge editor's plain helpers: no React, no icons, so the map
// logic and the tests can use them too.

import {
  fillLabel, getAt, toPointer,
  type AnnotationIndex, type FieldAnnotation, type Json, type JsonObject, type Overrides, type Seg,
} from '../model'

/** A route waypoint offered for a point's position. */
export interface PointOption {
  label: string
  x: number
  y: number
  lat: number
  lon: number
  elev_m: number
}

export interface Ctx {
  idx: AnnotationIndex
  defaults: Json
  overrides: Overrides
  /** Defaults with the overrides applied. */
  value: Json
  readOnly: boolean
  set: (segs: Seg[], v: Json) => void
  /** Several changes at once, e.g. a reorder renumbering many priorities. */
  setMany?: (pairs: [Seg[], Json][]) => void
  /** Pointers never shown as fields (the load switches). */
  hidden: Set<string>
  points: PointOption[]
}

// ── small helpers ─────────────────────────────────────────────────────────

export const isScalar = (v: Json | undefined): boolean => v === null || typeof v !== 'object'
export const isObj = (v: Json | undefined): v is JsonObject => v !== null && typeof v === 'object' && !Array.isArray(v)
export const isFlat = (v: Json | undefined): v is JsonObject => isObj(v) && Object.values(v).every(isScalar)

/** "BurstQuantity" -> "Burst quantity", "mirror_COMM1" -> "Mirror COMM1". */
export function humanise(key: string): string {
  const s = key
    .replace(/_/g, ' ')
    .replace(/([a-z])([A-Z0-9])/g, '$1 $2')
    .replace(/([A-Z]+)([A-Z][a-z])/g, '$1 $2')
    .trim()
  return s.charAt(0).toUpperCase() + s.slice(1).replace(/\b([A-Z])([a-z]+)\b/g, (_m, a: string, b: string) => a.toLowerCase() + b)
}

const natural = new Intl.Collator(undefined, { numeric: true, sensitivity: 'base' })

/** Children in the order a pilot expects: annotated key order if any,
 *  otherwise numeric-aware (Channel_2 before Channel_10). */
export function entriesOf(v: Json, a: FieldAnnotation | undefined): [Seg, Json][] {
  if (Array.isArray(v)) return v.map((x, i) => [i, x])
  if (!isObj(v)) return []
  const order = [
    ...(a?.keyOptions?.map(k => k.value) ?? []),
    ...(a?.keys?.map(k => (typeof k === 'string' ? k : k.value)) ?? []),
    ...(a?.fixedKeys ?? []),
  ]
  const keys = Object.keys(v)
  const rank = (k: string) => (order.includes(k) ? order.indexOf(k) : order.length)
  return keys
    .sort((x, y) => rank(x) - rank(y) || natural.compare(x, y))
    .map(k => [k, v[k]])
}

/** How many overrides sit at or under `segs`. */
export function changesUnder(ctx: Ctx, segs: Seg[]): number {
  const p = toPointer(segs)
  return Object.keys(ctx.overrides).filter(k => k === p || k.startsWith(p + '/') || p.startsWith(k + '/')).length
}

export function labelOf(ctx: Ctx, segs: Seg[]): string {
  const a = ctx.idx.get(segs)
  const k = segs[segs.length - 1]
  if (a?.label) return a.label
  const parent = ctx.idx.get(segs.slice(0, -1))
  if (parent?.keyLabels?.[String(k)]) return parent.keyLabels[String(k)]
  const opt = parent?.keyOptions?.find(o => o.value === String(k))
  if (opt) return opt.label
  if (typeof k === 'number') return `#${k + 1}`
  return humanise(String(k))
}

/** An element's display name: annotation template, else its name/id field. */
export function rowLabel(ctx: Ctx, parent: Seg[], k: Seg, x: Json): string {
  const a = ctx.idx.get(parent)
  if (typeof k === 'number' && a?.itemLabels?.[k]) return a.itemLabels[k]
  if (a?.itemLabel) return fillLabel(a.itemLabel, x, k) || `#${Number(k) + 1}`
  if (typeof k === 'string') {
    // A row is named by its own key ("Channel 1", "MAN1", "SA-10"), not by
    // the label every sibling shares.
    if (a?.keyLabels?.[k]) return a.keyLabels[k]
    const opt = a?.keyOptions?.find(o => o.value === k)
    return opt && opt.label !== k ? opt.label : humanise(k)
  }
  if (isObj(x)) {
    for (const f of ['name', 'Name', 'text', 'id', 'Number', 'number', 'Sequence']) {
      const n = x[f]
      if (typeof n === 'string' && n.trim()) return n
      if (typeof n === 'number') return String(n)
    }
  }
  return `${Number(k) + 1}`
}

// ── shape detection ───────────────────────────────────────────────────────

/** A list or dict of like flat records, or an empty list of them. */
export function isRecordTable(v: Json, a: FieldAnnotation | undefined): boolean {
  const kids = Array.isArray(v) ? v : isObj(v) ? Object.values(v) : null
  if (!kids) return false
  if (kids.length === 0) return Array.isArray(v) && isFlat(a?.item ?? null)
  if (!kids.every(k => isFlat(k))) return false
  if (kids.length === 1) return Array.isArray(v)
  const first = Object.keys(kids[0] as JsonObject)
  return kids.every(k => Object.keys(k as JsonObject).filter(x => first.includes(x)).length >= first.length * 0.7)
}

/** An object whose object-children are like flat records: a small matrix. */
export function isMatrix(v: Json): boolean {
  if (!isObj(v)) return false
  const objs = Object.values(v).filter(x => !isScalar(x))
  if (objs.length < 2 || !objs.every(isFlat)) return false
  const keys = Object.keys(objs[0] as JsonObject).join('|')
  return objs.every(o => Object.keys(o as JsonObject).join('|') === keys)
}

export const isMatrixList = (v: Json) => {
  const kids = Array.isArray(v) ? v : isObj(v) ? Object.values(v) : null
  return !!kids && kids.length > 0 && kids.every(isMatrix)
}

// ── flat lists for search and "changed only" ─────────────────────────────

export interface LeafHit { segs: Seg[]; path: string[] }

/** Every editable leaf with its label path, for search. */
export function collectLeaves(ctx: Ctx, segs: Seg[] = [], path: string[] = [], out: LeafHit[] = [], depth = 0): LeafHit[] {
  const v = getAt(ctx.value, segs) as Json | undefined
  if (v === undefined || depth > 10) return out
  const a = segs.length ? ctx.idx.get(segs) : undefined
  if (a?.hidden || a?.managed || ctx.hidden.has(toPointer(segs))) return out
  if (isScalar(v)) {
    if (segs.length) out.push({ segs, path })
    return out
  }
  for (const [k, x] of entriesOf(v, a)) {
    collectLeaves(ctx, [...segs, k], [...path, rowLabel(ctx, segs, k, x)], out, depth + 1)
  }
  return out
}

/** Sentences written for the exporter, not for a pilot. */
const DEV = /setData|returnData|JSON|throws?\b|\barray\b|wholesale|descriptor|\bnil\b|\bnulls?\b|terrain-gated|upload flags?\b|\bholes\b|\bids?\b|mirror_|\bLua\b|exporter|importer|merged|\bkeys?\b|\bindex\b|\.lua|Newtonsoft|constructor|serialis|deserialis|getter|uploader|\bUI\b/i
const HARD = /setData|returnData|JSON|throws?\b|exporter|importer|\bnil\b|Newtonsoft|\.lua|mirror_/i

/** Annotation help as a pilot should read it: implementation notes dropped,
 *  anything about the cockpit kept. */
export function playerHelp(text: string | undefined): string | undefined {
  if (!text) return undefined
  const kept = text
    .split(/(?<=[.!?])\s+/)
    .filter(s => !DEV.test(s) || (/cockpit|\bjet\b|pilot|in flight|HUD|MFD|DED|UFC/i.test(s) && !HARD.test(s)))
  const out = kept.join(' ').trim()
  return out || undefined
}

/** What a target can carry beyond the route itself. */
export interface OffsetSupport {
  /** Offset aimpoints per steerpoint. */
  oa: number
  /** VIP and VRP (F-16). */
  vipvrp: boolean
}

/** Which cartridges take offsets: the F-16 (native and DCS-DTC) and the
 *  native Hornet. */
export function offsetSupport(code: string): OffsetSupport | null {
  if (code === 'F-16C_50' || code === 'F16C') return { oa: 2, vipvrp: true }
  if (code === 'FA-18C_hornet') return { oa: 1, vipvrp: false }
  return null
}
