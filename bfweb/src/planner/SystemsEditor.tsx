import { memo, useMemo, useState, type CSSProperties } from 'react'
import { ChevronDown, ChevronRight, Plus, Trash2, RotateCcw } from '@icons'
import {
  AnnotationIndex, applyOverrides, checkValue, clone, deepEqual, fillLabel, getAt, toPointer, withOverride,
  type Annotations, type FieldAnnotation, type Json, type JsonObject, type Overrides, type Seg,
} from './model'

/**
 * Every setting a target holds, edited in place: the target's defaults with
 * the plan's overrides applied, laid out by its annotations. Collapsed until
 * opened -- the F-16's threat tables alone are hundreds of rows -- and only
 * changed values are kept.
 */
export interface SystemsEditorProps {
  defaults: Json
  annotations: Annotations
  overrides: Overrides
  readOnly: boolean
  onChange: (next: Overrides) => void
}

const input: CSSProperties = {
  padding: '3px 6px', fontSize: '0.7rem', minWidth: 0, fontFamily: 'var(--font-mono)',
}
const rowLabel: CSSProperties = {
  fontSize: '0.64rem', color: 'var(--text-dim)', minWidth: 0, overflow: 'hidden',
  textOverflow: 'ellipsis', whiteSpace: 'nowrap',
}

/** "BurstQuantity" -> "Burst quantity", "mirror_COMM1" -> "Mirror COMM1". */
export function humanise(key: string): string {
  const s = key
    .replace(/_/g, ' ')
    .replace(/([a-z])([A-Z])/g, '$1 $2')
    .replace(/([A-Z]+)([A-Z][a-z])/g, '$1 $2')
    .trim()
  return s.charAt(0).toUpperCase() + s.slice(1).replace(/\b([A-Z])([a-z]+)\b/g, (_m, a: string, b: string) => a.toLowerCase() + b)
}

interface Ctx {
  idx: AnnotationIndex
  defaults: Json
  overrides: Overrides
  value: Json
  readOnly: boolean
  set: (segs: Seg[], v: Json) => void
}

function isObj(v: Json | undefined): v is JsonObject {
  return v !== null && typeof v === 'object' && !Array.isArray(v)
}

function changed(ctx: Ctx, segs: Seg[]): boolean {
  const p = toPointer(segs)
  return Object.keys(ctx.overrides).some(k => k === p || k.startsWith(p + '/') || p.startsWith(k + '/'))
}

function Leaf({ ctx, segs, a, v }: { ctx: Ctx; segs: Seg[]; a?: FieldAnnotation; v: Json }) {
  const err = checkValue(a, v)
  const ro = ctx.readOnly || a?.readonly
  const set = (nv: Json) => ctx.set(segs, nv)
  const style = { ...input, borderColor: err ? '#f0a030' : undefined }
  const name = a?.label ?? humanise(String(segs[segs.length - 1]))

  let control
  if (a?.options?.length) {
    const i = a.options.findIndex(o => deepEqual(o.value, v))
    control = (
      <select className="vs-input" style={style} disabled={ro} value={i} aria-label={name}
        onChange={e => set(clone(a.options![Number(e.target.value)].value))}>
        {i < 0 && <option value={-1}>{JSON.stringify(v)}</option>}
        {a.options.map((o, j) => <option key={j} value={j}>{o.label}</option>)}
      </select>
    )
  } else if (typeof v === 'boolean') {
    control = <input type="checkbox" disabled={ro} checked={v} aria-label={name} onChange={e => set(e.target.checked)} />
  } else if (typeof v === 'number' && a?.display) {
    // Stored in one unit (DCS metres), edited in the one the cockpit shows.
    const d = a.display
    const shown = Number((v * d.factor).toFixed(d.decimals ?? 2))
    control = (
      <input className="vs-input" style={{ ...style, width: 96 }} disabled={ro} type="number" aria-label={name}
        min={a.min != null ? Math.round(a.min * d.factor) : undefined} max={a.max != null ? Math.round(a.max * d.factor) : undefined}
        step={d.decimals ? 10 ** -d.decimals : 1} value={shown}
        onChange={e => { const n = Number(e.target.value); if (e.target.value !== '' && Number.isFinite(n)) set(n / d.factor) }} />
    )
  } else if (typeof v === 'number') {
    control = (
      <input className="vs-input" style={{ ...style, width: 96 }} disabled={ro} type="number" aria-label={name}
        min={a?.min} max={a?.max} step={a?.step ?? (a?.kind === 'int' || Number.isInteger(v) ? 1 : 'any')} value={v}
        onChange={e => { const n = Number(e.target.value); if (e.target.value !== '' && Number.isFinite(n)) set(n) }} />
    )
  } else if (v === null && a?.kind === 'time') {
    control = (
      <input className="vs-input" style={{ ...style, width: 96 }} disabled={ro} placeholder="HH:MM:SS" aria-label={name}
        onChange={e => set(e.target.value || null)} />
    )
  } else if ((typeof v === 'string' || v === null) && a?.multiline) {
    control = (
      <textarea className="vs-input" style={{ ...style, flex: 1, minHeight: 48, resize: 'vertical' }} disabled={ro} aria-label={name}
        maxLength={a.maxLength} value={v ?? ''} onChange={e => set(e.target.value)} />
    )
  } else if (typeof v === 'string' || v === null) {
    control = (
      <input className="vs-input" style={{ ...style, flex: 1 }} disabled={ro} aria-label={name} value={v ?? ''} maxLength={a?.maxLength}
        placeholder={v === null ? 'null' : undefined}
        onChange={e => set(a?.kind === 'time' && e.target.value === '' ? null : e.target.value)} />
    )
  } else {
    control = <span style={{ ...input, color: 'var(--text-dim)' }}>{JSON.stringify(v)}</span>
  }

  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'minmax(90px, 40%) 1fr', gap: 6, alignItems: 'center', padding: '2px 0' }}
      title={a?.help}>
      <span style={{ ...rowLabel, color: changed(ctx, segs) ? 'var(--accent)' : rowLabel.color }}>
        {name}{a?.display ? ` (${a.display.unit})` : a?.unit ? ` (${a.unit})` : ''}
      </span>
      <span style={{ display: 'flex', gap: 6, alignItems: 'center', minWidth: 0 }}>
        {control}
        {err && <span style={{ fontSize: '0.58rem', color: '#f0a030' }}>{err}</span>}
      </span>
    </div>
  )
}

/** One element of an array of objects, or a nested object: a fold. */
const Node = memo(function Node({ ctx, segs, label, depth }: { ctx: Ctx; segs: Seg[]; label: string; depth: number }) {
  const v = getAt(ctx.value, segs)
  const a = ctx.idx.get(segs)
  const [open, setOpen] = useState(false)
  if (v === undefined || a?.hidden) return null

  if (a?.managed) {
    return (
      <div style={{ ...rowLabel, padding: '3px 0', fontStyle: 'italic' }} title={a.help}>
        {a.label ?? label}: {a.managed === 'route' ? 'filled from the route' : a.managed === 'terrain' ? 'set from the map' : 'set from the plan name'}
      </div>
    )
  }
  if (v === null && a?.nullable && a.item != null && typeof a.item === 'object') {
    return (
      <div style={{ display: 'flex', alignItems: 'center', gap: 6, padding: '2px 0' }} title={a.help}>
        <span style={rowLabel}>{a.label ?? label}: off</span>
        {!ctx.readOnly && (
          <button type="button" className="vs-btn-ghost" style={input} onClick={() => ctx.set(segs, clone(a.item!))}>Turn on</button>
        )}
      </div>
    )
  }
  if (v === null || typeof v !== 'object') return <Leaf ctx={ctx} segs={segs} a={a} v={v} />

  const isArr = Array.isArray(v)
  const entries: [Seg, Json][] = isArr ? (v as Json[]).map((x, i) => [i, x]) : Object.entries(v as JsonObject)
  const primitiveArr = isArr && entries.every(([, x]) => x === null || typeof x !== 'object')
  const canResize = isArr && !ctx.readOnly && !a?.readonly && a?.fixedLength == null
  const canAdd = canResize && (a?.maxItems == null || entries.length < a.maxItems)
  const canRemove = canResize && (a?.minItems == null || entries.length > a.minItems)
  const isChanged = changed(ctx, segs)
  // A dict with a known key set (stations, laser codes): entries can be
  // added from the keys not yet present, and removed unless the set is fixed.
  const dictKeys = !isArr ? dictKeyOptions(a) : []
  const missingKeys = dictKeys.filter(k => !(k.value in (v as JsonObject)))
  const dictEditable = !isArr && !ctx.readOnly && !a?.readonly && dictKeys.length > 0 && !a?.fixedKeys
  const childLabel = (k: Seg, x: Json) => {
    if (a?.itemLabel) return fillLabel(a.itemLabel, x) || `#${Number(k) + 1}`
    if (isArr) return itemLabel(x, Number(k))
    return a?.keyLabels?.[String(k)] ?? dictKeys.find(o => o.value === String(k))?.label ?? humanise(String(k))
  }

  return (
    <div style={{ borderLeft: depth > 0 ? '1px solid var(--border)' : undefined, paddingLeft: depth > 0 ? 8 : 0, margin: '2px 0' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
        <button type="button" onClick={() => setOpen(o => !o)} title={a?.help}
          style={{ display: 'flex', alignItems: 'center', gap: 4, background: 'none', border: 'none', color: isChanged ? 'var(--accent)' : 'var(--text)', cursor: 'pointer', padding: '2px 0', fontSize: depth === 0 ? '0.74rem' : '0.68rem', fontWeight: depth === 0 ? 700 : 600, textAlign: 'left' }}>
          {open ? <ChevronDown size={11} /> : <ChevronRight size={11} />}
          {a?.label ?? label}
          <span style={{ fontWeight: 400, color: 'var(--text-dim)', fontSize: '0.6rem' }}>
            {isArr ? `${entries.length} item${entries.length === 1 ? '' : 's'}` : ''}
          </span>
        </button>
        {a?.nullable && !ctx.readOnly && (
          <button type="button" className="vs-btn-ghost" style={{ ...input, padding: '1px 5px' }} title="Set to null" onClick={() => ctx.set(segs, null)}>off</button>
        )}
        {isChanged && !ctx.readOnly && (
          <button type="button" className="vs-btn-ghost" style={{ padding: 2 }} title="Reset to default"
            onClick={() => ctx.set(segs, clone(getAt(ctx.defaults, segs) ?? (isArr ? [] : {})))}>
            <RotateCcw size={10} />
          </button>
        )}
      </div>
      {open && (
        <div style={{ paddingLeft: 4 }}>
          {primitiveArr ? (
            entries.map(([k, x]) => <Leaf key={String(k)} ctx={ctx} segs={[...segs, k]} a={ctx.idx.get([...segs, k])} v={x} />)
          ) : (
            entries.map(([k, x]) => (
              <div key={String(k)} style={{ display: 'flex', alignItems: 'flex-start', gap: 4 }}>
                <div style={{ flex: 1, minWidth: 0 }}>
                  <Node ctx={ctx} segs={[...segs, k]} depth={depth + 1} label={childLabel(k, x)} />
                </div>
                {canRemove && (
                  <button type="button" className="vs-btn-ghost" style={{ padding: 2, marginTop: 2 }} title="Remove"
                    onClick={() => ctx.set(segs, (v as Json[]).filter((_, j) => j !== k))}><Trash2 size={10} /></button>
                )}
                {dictEditable && (
                  <button type="button" className="vs-btn-ghost" style={{ padding: 2, marginTop: 2 }} title="Remove"
                    onClick={() => {
                      const next = { ...(v as JsonObject) }
                      delete next[String(k)]
                      ctx.set(segs, next)
                    }}><Trash2 size={10} /></button>
                )}
              </div>
            ))
          )}
          {canAdd && (
            <button type="button" className="vs-btn-ghost" style={{ ...input, display: 'flex', gap: 4, alignItems: 'center', margin: '3px 0' }}
              onClick={() => {
                const arr = v as Json[]
                const tmpl = a?.item !== undefined ? clone(a.item) : arr.length ? clone(arr[arr.length - 1]) : templateFor(ctx, segs)
                ctx.set(segs, [...arr, tmpl])
              }}>
              <Plus size={10} /> add
            </button>
          )}
          {dictEditable && missingKeys.length > 0 && (
            <select className="vs-input" style={{ ...input, margin: '3px 0' }} value="" aria-label="Add an entry"
              onChange={e => {
                const key = e.target.value
                if (!key) return
                const opt = missingKeys.find(o => o.value === key)
                const fresh = opt?.default !== undefined ? clone(opt.default) : a?.item !== undefined ? clone(a.item) : {}
                ctx.set(segs, { ...(v as JsonObject), [key]: fresh })
              }}>
              <option value="">+ add…</option>
              {missingKeys.map(o => <option key={o.value} value={o.value}>{o.label}</option>)}
            </select>
          )}
        </div>
      )}
    </div>
  )
})

/** The keys a dict annotation allows, labelled. */
function dictKeyOptions(a: FieldAnnotation | undefined): { value: string; label: string; default?: Json }[] {
  if (!a) return []
  if (a.keyOptions?.length) return a.keyOptions
  return (a.keys ?? []).map(k => (typeof k === 'string' ? { value: k, label: k } : k))
}

/** A readable name for one element of an object array. */
function itemLabel(x: Json, i: number): string {
  if (isObj(x)) {
    for (const k of ['name', 'Name', 'id', 'Id', 'text', 'Text', 'Number', 'number', 'Sequence', 'label']) {
      const n = x[k]
      if (typeof n === 'string' && n.trim()) return `${i + 1} · ${n}`
      if (typeof n === 'number') return `${i + 1} · ${k} ${n}`
    }
  }
  return `#${i + 1}`
}

/** An empty array's element shape, borrowed from the defaults. */
function templateFor(ctx: Ctx, segs: Seg[]): Json {
  const d = getAt(ctx.defaults, segs)
  return Array.isArray(d) && d.length ? clone(d[0]) : {}
}

export default function SystemsEditor({ defaults, annotations, overrides, readOnly, onChange }: SystemsEditorProps) {
  const idx = useMemo(() => new AnnotationIndex(annotations), [annotations])
  const value = useMemo(() => applyOverrides(defaults, overrides), [defaults, overrides])
  const ctx: Ctx = {
    idx, defaults, overrides, value, readOnly,
    set: (segs, v) => onChange(withOverride(defaults, overrides, segs, v)),
  }
  // Sections: the annotated order first, then any other top-level key.
  const top = isObj(value) ? Object.keys(value) : []
  const ordered = [
    ...(annotations.sections ?? []).map(s => s.path).filter(p => top.includes(p)),
    ...top.filter(k => !(annotations.sections ?? []).some(s => s.path === k)),
  ]
  const n = Object.keys(overrides).length
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', fontSize: '0.62rem', color: 'var(--text-dim)' }}>
        <span>{n ? `${n} change${n === 1 ? '' : 's'} from the defaults` : 'All defaults'} · hover a field for help</span>
        {n > 0 && !readOnly && (
          <button type="button" className="vs-btn-ghost" style={input} onClick={() => onChange({})}>Reset all</button>
        )}
      </div>
      {ordered.map(k => {
        const s = idx.section(k)
        return <Node key={k} ctx={ctx} segs={[k]} depth={0} label={s?.label ?? humanise(k)} />
      })}
    </div>
  )
}
