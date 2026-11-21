// The part of MapLibre's style language the dashboard's map layers use,
// evaluated in plain JavaScript, so lib/leafletgl can draw the same layers
// with Leaflet where the browser has no WebGL (DCS's embedded one).
//
// Covered: get, has, geometry-type, zoom, line-progress, literal, match,
// case, coalesce, step, interpolate (linear, exponential; numbers and
// colours), ==, !=, <, <=, >, >=, !, all, any, + - * / min max, to-string,
// concat, plus the legacy filter form (["==", "key", value]). Anything else
// evaluates to null, which every caller treats as "use the default".

import type { Feature } from 'geojson'

export interface EvalCtx {
  zoom: number
  feature?: Feature
  /** 0..1 along a line, for line-gradient. */
  progress?: number
}

type Expr = unknown

function geometryType(f: Feature | undefined): string | null {
  const t = f?.geometry?.type
  if (!t) return null
  return t.replace(/^Multi/, '')
}

function isExpr(v: unknown): v is [string, ...unknown[]] {
  return Array.isArray(v) && typeof v[0] === 'string'
}

/** CSS colour -> [r, g, b, a] (0..255, 0..1), or null. Hex and rgb()/rgba(). */
export function parseColor(c: unknown): [number, number, number, number] | null {
  if (typeof c !== 'string') return null
  const s = c.trim()
  if (s.startsWith('#')) {
    let h = s.slice(1)
    if (h.length === 3 || h.length === 4) h = [...h].map((x) => x + x).join('')
    if (h.length !== 6 && h.length !== 8) return null
    const n = parseInt(h, 16)
    if (Number.isNaN(n)) return null
    if (h.length === 6) return [(n >> 16) & 255, (n >> 8) & 255, n & 255, 1]
    return [(n >>> 24) & 255, (n >> 16) & 255, (n >> 8) & 255, (n & 255) / 255]
  }
  const m = /^rgba?\(([^)]+)\)$/i.exec(s)
  if (!m) return null
  const p = m[1].split(',').map((x) => parseFloat(x))
  if (p.length < 3 || p.some((x) => Number.isNaN(x))) return null
  return [p[0], p[1], p[2], p.length > 3 ? p[3] : 1]
}

function rgba(c: [number, number, number, number]): string {
  return `rgba(${Math.round(c[0])},${Math.round(c[1])},${Math.round(c[2])},${+c[3].toFixed(3)})`
}

function interpolate(kind: Expr, input: number, stops: unknown[]): unknown {
  const pairs: [number, unknown][] = []
  for (let i = 0; i + 1 < stops.length; i += 2) pairs.push([Number(stops[i]), stops[i + 1]])
  if (!pairs.length) return null
  if (input <= pairs[0][0]) return pairs[0][1]
  if (input >= pairs[pairs.length - 1][0]) return pairs[pairs.length - 1][1]
  let i = 0
  while (i + 1 < pairs.length && pairs[i + 1][0] < input) i++
  const [z0, v0] = pairs[i]
  const [z1, v1] = pairs[i + 1]
  const base = isExpr(kind) && kind[0] === 'exponential' ? Number(kind[1]) || 1 : 1
  const t = base === 1
    ? (input - z0) / (z1 - z0)
    : (Math.pow(base, input - z0) - 1) / (Math.pow(base, z1 - z0) - 1)
  if (typeof v0 === 'number' && typeof v1 === 'number') return v0 + (v1 - v0) * t
  const c0 = parseColor(v0)
  const c1 = parseColor(v1)
  if (c0 && c1) return rgba([0, 1, 2, 3].map((k) => c0[k] + (c1[k] - c0[k]) * t) as [number, number, number, number])
  return t < 0.5 ? v0 : v1
}

const num = (v: unknown) => (typeof v === 'number' ? v : Number(v))

/** Evaluate a style value: a literal, or an expression. */
export function evaluate(e: Expr, ctx: EvalCtx): unknown {
  if (!isExpr(e)) return e
  const [op, ...a] = e
  const ev = (x: Expr) => evaluate(x, ctx)
  const props = (ctx.feature?.properties ?? {}) as Record<string, unknown>
  switch (op) {
    case 'literal': return a[0]
    case 'get': return props[String(ev(a[0]))] ?? null
    case 'has': return String(ev(a[0])) in props
    case 'geometry-type': return geometryType(ctx.feature)
    case 'zoom': return ctx.zoom
    case 'line-progress': return ctx.progress ?? 1
    case 'match': {
      const v = ev(a[0])
      for (let i = 1; i + 1 < a.length; i += 2) {
        const label = a[i]
        const hit = Array.isArray(label) ? label.includes(v as never) : label === v
        if (hit) return ev(a[i + 1])
      }
      return a.length % 2 === 0 ? ev(a[a.length - 1]) : null
    }
    case 'case': {
      for (let i = 0; i + 1 < a.length; i += 2) if (ev(a[i])) return ev(a[i + 1])
      return a.length % 2 === 1 ? ev(a[a.length - 1]) : null
    }
    case 'coalesce': {
      for (const x of a) {
        const v = ev(x)
        if (v != null) return v
      }
      return null
    }
    case 'step': {
      const input = num(ev(a[0]))
      let out = ev(a[1])
      for (let i = 2; i + 1 < a.length; i += 2) if (input >= num(a[i])) out = ev(a[i + 1])
      return out
    }
    case 'interpolate': return interpolate(a[0], num(ev(a[1])), a.slice(2).map((x, i) => (i % 2 === 0 ? x : ev(x))))
    case '==': return ev(a[0]) === ev(a[1])
    case '!=': return ev(a[0]) !== ev(a[1])
    case '<': return num(ev(a[0])) < num(ev(a[1]))
    case '<=': return num(ev(a[0])) <= num(ev(a[1]))
    case '>': return num(ev(a[0])) > num(ev(a[1]))
    case '>=': return num(ev(a[0])) >= num(ev(a[1]))
    case '!': return !ev(a[0])
    case 'all': return a.every((x) => !!ev(x))
    case 'any': return a.some((x) => !!ev(x))
    case '+': return a.reduce<number>((s, x) => s + num(ev(x)), 0)
    case '*': return a.reduce<number>((s, x) => s * num(ev(x)), 1)
    case '-': return a.length === 1 ? -num(ev(a[0])) : num(ev(a[0])) - num(ev(a[1]))
    case '/': return num(ev(a[0])) / num(ev(a[1]))
    case 'max': return Math.max(...a.map((x) => num(ev(x))))
    case 'min': return Math.min(...a.map((x) => num(ev(x))))
    case 'to-string': return String(ev(a[0]) ?? '')
    case 'concat': return a.map((x) => String(ev(x) ?? '')).join('')
    default: return null
  }
}

/** Does `f` pass a layer filter? Accepts expressions and the legacy form. */
export function passes(filter: unknown, f: Feature, zoom: number): boolean {
  if (filter == null) return true
  if (!Array.isArray(filter)) return !!filter
  const [op, k, ...rest] = filter as unknown[]
  // Legacy: ["==", "key", value], ["in", "key", ...], ["has", "key"]; "$type" is the geometry.
  if (typeof k === 'string' && !isExpr(k) && ['==', '!=', 'in', '!in', 'has', '!has'].includes(String(op)) && filter.length >= 2) {
    const props = (f.properties ?? {}) as Record<string, unknown>
    const v = k === '$type' ? geometryType(f) : props[k]
    switch (op) {
      case '==': return v === rest[0]
      case '!=': return v !== rest[0]
      case 'in': return rest.includes(v)
      case '!in': return !rest.includes(v)
      case 'has': return k in props
      case '!has': return !(k in props)
    }
  }
  return !!evaluate(filter, { zoom, feature: f })
}

/** A paint/layout value as a number, or `dflt`. */
export function numberOf(v: unknown, ctx: EvalCtx, dflt: number): number {
  const r = evaluate(v, ctx)
  const n = typeof r === 'number' ? r : Number(r)
  return r == null || Number.isNaN(n) ? dflt : n
}

/** A paint value as a CSS colour string, or `dflt`. */
export function colorOf(v: unknown, ctx: EvalCtx, dflt: string): string {
  const r = evaluate(v, ctx)
  return typeof r === 'string' && r ? r : dflt
}

/** MapLibre's line-dasharray is in line widths; Leaflet's dashArray in px. */
export function dashArray(v: unknown, ctx: EvalCtx, width: number): string | undefined {
  const r = evaluate(v, ctx)
  if (!Array.isArray(r) || !r.length) return undefined
  const w = Math.max(width, 1)
  const parts = r.map((x) => Math.max(0, Number(x) * w))
  // A leading zero (MapLibre's "start with a gap" trick for animated dashes)
  // is a zero-length dash: SVG takes it as written.
  return parts.map((x) => +x.toFixed(2)).join(' ')
}
