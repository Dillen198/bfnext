// The avionics model behind every export target. Each target (a native DCS
// cartridge or a DCS-DTC preset for one aircraft) is described by:
//
// * `defaults` -- the file a fresh cartridge/preset would be, generated from
//   DCS's own descriptors or DCS-DTC's own constructors;
// * `annotations` -- labels, units, ranges and option lists keyed by path
//   pattern, which drive the editor.
//
// A plan stores only what its author changed: a map from JSON Pointer to the
// new value. That keeps a plan small (the F-16's defaults alone are ~300 KB
// of threat tables) and lets a newer DCS's defaults show through untouched
// fields.

export type Json = null | boolean | number | string | Json[] | { [k: string]: Json }
export type JsonObject = { [k: string]: Json }

/** JSON Pointer path -> replacement value. */
export type Overrides = Record<string, Json>

export type FieldKind =
  | 'int' | 'number' | 'bool' | 'string' | 'enum' | 'freq' | 'time'
  | 'coord-lat' | 'coord-lon' | 'coord-lat-dms' | 'coord-lon-dms'
  // containers
  | 'list' | 'dict' | 'object'

export interface FieldAnnotation {
  /** Pattern: dot-separated keys, `[]` any array index, `{}` any key. */
  path: string
  label?: string
  kind?: FieldKind
  min?: number
  max?: number
  unit?: string
  pattern?: string
  help?: string
  options?: { value: Json; label: string }[]
  readonly?: boolean
  hidden?: boolean
  /** Filled from the plan's route by the exporter; not edited here. */
  managed?: 'route' | 'terrain' | 'name'
  /** Array whose length must not change. */
  fixedLength?: number
  nullCrashesEditor?: boolean
  /** Containers: template for a new element, and how many there may be. */
  item?: Json
  minItems?: number
  maxItems?: number
  /** May be null (a nullable object: off until switched on with `item`). */
  nullable?: boolean
  /** Dict keys that must be exactly these. */
  fixedKeys?: string[]
  /** Keys a dict may hold: plain, or labelled. */
  keys?: (string | { value: string; label: string })[]
  /** Labelled keys, each with the value a new entry starts at. */
  keyOptions?: { value: string; label: string; default?: Json }[]
  maxLength?: number
  multiline?: boolean
  step?: number
  /** The shipped default may sit outside min/max; don't flag it. */
  allowDefaultOutOfRange?: boolean
  dependsOn?: string
  /** Stored in one unit, shown in another: shown = stored * factor. */
  display?: { unit: string; factor: number; decimals?: number }
  /** Label for an element of this list/dict: "W{num}" fills from its fields. */
  itemLabel?: string
  /** The element field that names a dict entry. */
  keyField?: string
  keyLabels?: Record<string, string>
}

/** An annotation file as shipped, in either dialect. */
export interface RawAnnotations extends Omit<Annotations, 'sections' | 'derived'> {
  sections?: (Partial<FieldAnnotation> & { path: string; label: string; help?: string; template?: Json })[]
  derived?: (string | { path?: string; formula?: string })[]
}

/** The two annotation dialects (DCS-DTC and native) into one: native files
 *  put container rules on nested "sections" with `template`; here those
 *  become field annotations with `item`, and only top-level sections stay. */
export function normaliseAnnotations(raw: RawAnnotations): Annotations {
  const top: NonNullable<Annotations['sections']> = []
  const fields: FieldAnnotation[] = []
  for (const s of raw.sections ?? []) {
    const { template, ...rest } = s
    if (!s.path.includes('.') && !s.path.includes('[') && !s.path.includes('{')) {
      top.push({ path: s.path, label: s.label, help: s.help })
      // Section-level rules (managed, hidden...) still apply to the node.
      if (rest.managed || rest.hidden || rest.readonly || template !== undefined || rest.maxItems != null || rest.fixedLength != null) {
        fields.push({ ...rest, ...(template !== undefined ? { item: template } : {}) })
      }
    } else {
      fields.push({ ...rest, ...(template !== undefined ? { item: template } : {}) })
    }
  }
  const derived = (raw.derived ?? []).map(d => (typeof d === 'string' ? d : `${d.path ?? ''}: ${d.formula ?? ''}`))
  // A field annotation beats a section-derived one on the same path.
  const seen = new Set(raw.fields.map(f => f.path))
  return { ...raw, sections: top, fields: [...raw.fields, ...fields.filter(f => !seen.has(f.path))], derived }
}

/** "W{num}" with an element's fields filled in. */
export function fillLabel(tmpl: string, el: Json): string {
  return tmpl.replace(/\{(\w+)\}/g, (_, k: string) => {
    const v = el !== null && typeof el === 'object' && !Array.isArray(el) ? (el as JsonObject)[k] : undefined
    return v == null ? '' : String(v)
  })
}

export interface Annotations {
  code: string
  label: string
  sections?: { path: string; label: string; help?: string }[]
  fields: FieldAnnotation[]
  /** How the exporter computes hidden fields (documentation only). */
  derived?: string[]
}

export type Seg = string | number

// ── JSON Pointer ──────────────────────────────────────────────────────────

const esc = (s: string) => s.replace(/~/g, '~0').replace(/\//g, '~1')
const unesc = (s: string) => s.replace(/~1/g, '/').replace(/~0/g, '~')

export function toPointer(segs: Seg[]): string {
  return segs.map(s => '/' + esc(String(s))).join('')
}

export function fromPointer(p: string): string[] {
  return p === '' ? [] : p.slice(1).split('/').map(unesc)
}

export function clone<T extends Json>(v: T): T {
  return v === null || typeof v !== 'object' ? v : (JSON.parse(JSON.stringify(v)) as T)
}

export function getAt(root: Json, segs: Seg[]): Json | undefined {
  let cur: Json | undefined = root
  for (const s of segs) {
    if (cur === null || typeof cur !== 'object') return undefined
    cur = Array.isArray(cur) ? cur[Number(s)] : (cur as JsonObject)[String(s)]
  }
  return cur
}

/** Set in place, creating objects along the way. */
export function setAt(root: Json, segs: Seg[], value: Json): void {
  let cur = root as JsonObject | Json[]
  segs.forEach((s, i) => {
    const last = i === segs.length - 1
    if (Array.isArray(cur)) {
      const k = Number(s)
      if (last) cur[k] = value
      else {
        if (cur[k] === null || typeof cur[k] !== 'object') cur[k] = {}
        cur = cur[k] as JsonObject
      }
    } else {
      const k = String(s)
      if (last) cur[k] = value
      else {
        if (cur[k] === null || typeof cur[k] !== 'object') cur[k] = {}
        cur = cur[k] as JsonObject
      }
    }
  })
}

export function deepEqual(a: Json | undefined, b: Json | undefined): boolean {
  if (a === b) return true
  if (a === null || b === null || typeof a !== 'object' || typeof b !== 'object') return false
  if (Array.isArray(a) !== Array.isArray(b)) return false
  if (Array.isArray(a)) {
    const bb = b as Json[]
    return a.length === bb.length && a.every((v, i) => deepEqual(v, bb[i]))
  }
  const ka = Object.keys(a)
  const kb = Object.keys(b as JsonObject)
  return ka.length === kb.length && ka.every(k => deepEqual((a as JsonObject)[k], (b as JsonObject)[k]))
}

/** Defaults with the overrides applied, shortest path first so a whole-array
 *  override is laid down before edits inside it. */
export function applyOverrides<T extends Json>(defaults: T, ov: Overrides): T {
  const out = clone(defaults)
  const paths = Object.keys(ov).sort((a, b) => fromPointer(a).length - fromPointer(b).length)
  for (const p of paths) {
    const segs = fromPointer(p)
    if (segs.length === 0) continue
    setAt(out, segs, clone(ov[p]))
  }
  return out
}

/**
 * The overrides that still fit `defaults`. A path must lead through existing
 * containers to a key the defaults have -- or into an array, an empty
 * object (a dict that starts empty), or something an earlier override
 * created. Anything else is a typo or a layout a newer DCS/DCS-DTC changed,
 * and applying it would only add a key the importer ignores.
 */
export function pruneOverrides(defaults: Json, ov: Overrides): Overrides {
  const out: Overrides = {}
  const paths = Object.keys(ov).sort((a, b) => fromPointer(a).length - fromPointer(b).length)
  let cur = clone(defaults)
  for (const p of paths) {
    const segs = fromPointer(p)
    if (!segs.length) continue
    const parent = getAt(cur, segs.slice(0, -1))
    if (parent === null || typeof parent !== 'object') continue
    const key = segs[segs.length - 1]
    const fits = Array.isArray(parent)
      ? /^\d+$/.test(key)
      : key in parent || Object.keys(parent).length === 0 || Object.keys(out).some(k => p.startsWith(k + '/'))
    if (!fits) continue
    out[p] = ov[p]
    cur = applyOverrides(defaults, out)
  }
  return out
}

/** Record `value` at `segs`, dropping the override (and any inside it) when
 *  it puts the field back to the default. */
export function withOverride(defaults: Json, ov: Overrides, segs: Seg[], value: Json): Overrides {
  const p = toPointer(segs)
  const next: Overrides = {}
  for (const [k, v] of Object.entries(ov)) {
    if (k !== p && !k.startsWith(p + '/')) next[k] = v
  }
  // The field's value under the remaining overrides: an enclosing whole-array
  // override counts as the baseline, not the raw default.
  const base = getAt(applyOverrides(defaults, next), segs)
  if (!deepEqual(base, value)) next[p] = clone(value)
  return next
}

// ── Annotation matching ───────────────────────────────────────────────────

/** "CMS.Programs[].Chaff" -> ["CMS", "Programs", "[]", "Chaff"] */
export function patternSegs(pattern: string): string[] {
  const out: string[] = []
  for (const part of pattern.split('.')) {
    // A key may carry wildcard suffixes: "Programs[]", "Stations{}", "M[][]".
    const m = /^(.*?)((?:\[\]|\{\})*)$/.exec(part)!
    if (m[1]) out.push(m[1])
    for (const w of m[2].match(/\[\]|\{\}/g) ?? []) out.push(w)
  }
  return out
}

export function matches(pattern: string[], segs: Seg[]): boolean {
  if (pattern.length !== segs.length) return false
  return pattern.every((p, i) => {
    const s = segs[i]
    if (p === '[]') return typeof s === 'number' || /^\d+$/.test(String(s))
    if (p === '{}') return true
    return p === String(s)
  })
}

export class AnnotationIndex {
  private fields: { segs: string[]; a: FieldAnnotation }[]
  readonly ann: Annotations
  constructor(ann: Annotations) {
    this.ann = ann
    this.fields = ann.fields.map(a => ({ segs: patternSegs(a.path), a }))
  }
  /** The most specific annotation for `segs` (fewest wildcards). */
  get(segs: Seg[]): FieldAnnotation | undefined {
    let best: { a: FieldAnnotation; w: number } | undefined
    for (const f of this.fields) {
      if (!matches(f.segs, segs)) continue
      const w = f.segs.filter(s => s === '[]' || s === '{}').length
      if (!best || w < best.w) best = { a: f.a, w }
    }
    return best?.a
  }
  section(path: string) {
    return this.ann.sections?.find(s => s.path === path)
  }
}

/** Check a value against its annotation; a message, or null when fine. */
export function checkValue(a: FieldAnnotation | undefined, v: Json): string | null {
  if (!a) return null
  if (typeof v === 'number') {
    if (a.kind === 'int' && !Number.isInteger(v)) return 'whole number'
    if (a.min != null && v < a.min) return `min ${a.min}`
    if (a.max != null && v > a.max) return `max ${a.max}`
  }
  if (typeof v === 'string' && a.maxLength != null && v.length > a.maxLength) return `max ${a.maxLength} chars`
  if (typeof v === 'string' && a.pattern) {
    try { if (!new RegExp(a.pattern).test(v)) return 'format' } catch { /* bad pattern: skip */ }
  }
  if (a.kind === 'enum' && a.options && !a.options.some(o => deepEqual(o.value, v))) return 'not an option'
  return null
}
