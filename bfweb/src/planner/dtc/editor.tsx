// The cartridge editor's building blocks. A cartridge is a deep JSON tree;
// pilots think in cockpit pages and tables. So a section is shown as:
//
// * a table, when it is a list of like records (radio channels, RWR threats,
//   destination points, DCS-DTC CMS programs);
// * a grid card per item, when each item is a small matrix (a native CMDS
//   program: Chaff/Flare/Other x burst/salvo quantity/interval);
// * plain labelled fields otherwise, with sub-sections folded.
//
// Only what the pilot changes is stored (see model.ts); everything else shows
// the target's defaults.

import { useState, type CSSProperties, type ReactNode } from 'react'
import { ChevronDown, ChevronRight, Plus, RotateCcw, Trash2 } from '@icons'
import {
  checkValue, clone, deepEqual, fillLabel, getAt, toPointer,
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
  /** Pointers never shown as fields (the load switches). */
  hidden: Set<string>
  points: PointOption[]
}

// ── small helpers ─────────────────────────────────────────────────────────

export const isScalar = (v: Json | undefined): boolean => v === null || typeof v !== 'object'
export const isObj = (v: Json | undefined): v is JsonObject => v !== null && typeof v === 'object' && !Array.isArray(v)
const isFlat = (v: Json | undefined): v is JsonObject => isObj(v) && Object.values(v).every(isScalar)

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

// ── controls ──────────────────────────────────────────────────────────────

const inputStyle: CSSProperties = {
  padding: '3px 6px', fontSize: '0.72rem', minWidth: 0, fontFamily: 'var(--font-mono)',
}

/** The input for one value, sized to fit a table cell or a field row. */
export function Control({ ctx, segs, compact }: { ctx: Ctx; segs: Seg[]; compact?: boolean }) {
  const v = getAt(ctx.value, segs) as Json
  const a = ctx.idx.get(segs)
  const ro = ctx.readOnly || !!a?.readonly
  const set = (nv: Json) => ctx.set(segs, nv)
  const err = checkValue(a, v)
  const name = labelOf(ctx, segs)
  const style: CSSProperties = { ...inputStyle, borderColor: err ? '#f0a030' : undefined, width: compact ? '100%' : undefined }
  const title = [a?.help, err ? `Check: ${err}` : ''].filter(Boolean).join('\n') || undefined

  if (a?.options?.length) {
    const i = a.options.findIndex(o => deepEqual(o.value, v))
    return (
      <select className="vs-input" style={{ ...style, fontFamily: 'inherit' }} disabled={ro} value={i} aria-label={name} title={title}
        onChange={e => set(clone(a.options![Number(e.target.value)].value))}>
        {i < 0 && <option value={-1}>{v === null ? '—' : String(v)}</option>}
        {a.options.map((o, j) => <option key={j} value={j}>{o.label}</option>)}
      </select>
    )
  }
  if (typeof v === 'boolean') {
    return <input type="checkbox" disabled={ro} checked={v} aria-label={name} title={title} onChange={e => set(e.target.checked)} style={{ width: 15, height: 15 }} />
  }
  if (typeof v === 'number') {
    const d = a?.display
    const shown = d ? Number((v * d.factor).toFixed(d.decimals ?? 2)) : v
    return (
      <input className="vs-input" style={{ ...style, width: compact ? '100%' : 96 }} disabled={ro} type="number" aria-label={name} title={title}
        step={d ? (d.decimals ? 10 ** -d.decimals : 1) : a?.step ?? (a?.kind === 'int' || Number.isInteger(v) ? 1 : 'any')}
        value={shown}
        onChange={e => {
          const n = Number(e.target.value)
          if (e.target.value !== '' && Number.isFinite(n)) set(d ? n / d.factor : n)
        }} />
    )
  }
  if ((typeof v === 'string' || v === null) && a?.multiline) {
    return (
      <textarea className="vs-input" style={{ ...style, width: '100%', minHeight: 48, resize: 'vertical', fontFamily: 'inherit' }} disabled={ro}
        aria-label={name} title={title} maxLength={a.maxLength} value={v ?? ''} onChange={e => set(e.target.value)} />
    )
  }
  if (typeof v === 'string' || v === null) {
    return (
      <input className="vs-input" style={{ ...style, width: compact ? '100%' : undefined, flex: compact ? undefined : 1 }} disabled={ro}
        aria-label={name} title={title} value={v ?? ''} maxLength={a?.maxLength}
        placeholder={a?.kind === 'time' ? 'HH:MM:SS' : v === null ? '—' : undefined}
        onChange={e => set(e.target.value === '' && (v === null || a?.kind === 'time') ? null : e.target.value)} />
    )
  }
  return <span style={{ ...inputStyle, color: 'var(--text-dim)' }}>{JSON.stringify(v)}</span>
}

export function ResetButton({ ctx, segs }: { ctx: Ctx; segs: Seg[] }) {
  if (ctx.readOnly || !changesUnder(ctx, segs)) return null
  return (
    <button type="button" className="vs-btn-ghost" style={{ padding: 2, lineHeight: 0 }} title="Back to the default"
      onClick={() => ctx.set(segs, clone(getAt(ctx.defaults, segs) ?? null))}>
      <RotateCcw size={10} />
    </button>
  )
}

/** A labelled field: name (and unit) on the left, the control on the right. */
export function FieldRow({ ctx, segs, label }: { ctx: Ctx; segs: Seg[]; label?: string }) {
  const a = ctx.idx.get(segs)
  const unit = a?.display?.unit ?? a?.unit
  const changed = changesUnder(ctx, segs) > 0
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'minmax(120px, 42%) 1fr', gap: 8, alignItems: 'center', padding: '3px 0' }}>
      <span title={a?.help} style={{ fontSize: '0.7rem', color: changed ? 'var(--accent)' : 'var(--text-muted, var(--text-dim))', lineHeight: 1.3 }}>
        {label ?? labelOf(ctx, segs)}{unit ? <span style={{ color: 'var(--text-dim)' }}> ({unit})</span> : null}
        {a?.help && <span style={{ color: 'var(--text-dim)', marginLeft: 4, cursor: 'help' }} aria-hidden>ⓘ</span>}
      </span>
      <span style={{ display: 'flex', gap: 6, alignItems: 'center', minWidth: 0 }}>
        <Control ctx={ctx} segs={segs} />
        <ResetButton ctx={ctx} segs={segs} />
      </span>
    </div>
  )
}

// ── shape detection ───────────────────────────────────────────────────────

/** A list or dict of like flat records, or an empty list of them. */
function isRecordTable(v: Json, a: FieldAnnotation | undefined): boolean {
  const kids = Array.isArray(v) ? v : isObj(v) ? Object.values(v) : null
  if (!kids) return false
  if (kids.length === 0) return Array.isArray(v) && isFlat(a?.item ?? null)
  if (!kids.every(k => isFlat(k))) return false
  if (kids.length === 1) return Array.isArray(v)
  const first = Object.keys(kids[0] as JsonObject)
  return kids.every(k => Object.keys(k as JsonObject).filter(x => first.includes(x)).length >= first.length * 0.7)
}

/** An object whose object-children are like flat records: a small matrix. */
function isMatrix(v: Json): boolean {
  if (!isObj(v)) return false
  const objs = Object.values(v).filter(x => !isScalar(x))
  if (objs.length < 2 || !objs.every(isFlat)) return false
  const keys = Object.keys(objs[0] as JsonObject).join('|')
  return objs.every(o => Object.keys(o as JsonObject).join('|') === keys)
}

const isMatrixList = (v: Json) => {
  const kids = Array.isArray(v) ? v : isObj(v) ? Object.values(v) : null
  return !!kids && kids.length > 0 && kids.every(isMatrix)
}

// ── views ─────────────────────────────────────────────────────────────────

const th: CSSProperties = {
  textAlign: 'left', fontWeight: 600, fontSize: '0.62rem', color: 'var(--text-dim)', padding: '4px 6px',
  borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap', position: 'sticky', top: 0, background: 'var(--bg-card)', zIndex: 1,
}
const td: CSSProperties = { padding: '2px 4px', borderBottom: '1px solid var(--border)', verticalAlign: 'middle' }

/** "Chaff burst interval" under a "Chaff" row -> "Burst interval". */
function shortLabel(label: string, rowWords: string[]): string {
  let s = label
  for (const w of rowWords) {
    if (w && s.toLowerCase().startsWith(w.toLowerCase() + ' ')) s = s.slice(w.length + 1)
  }
  return s.charAt(0).toUpperCase() + s.slice(1)
}

/** Columns of a record table: the union of its rows' keys, minus hidden ones. */
function columnsOf(ctx: Ctx, segs: Seg[], rows: [Seg, Json][], template: Json | undefined): string[] {
  const cols: string[] = []
  const sample = rows.length ? rows.map(r => r[1] as JsonObject) : isObj(template ?? null) ? [template as JsonObject] : []
  for (const r of sample) for (const k of Object.keys(r)) if (!cols.includes(k)) cols.push(k)
  const firstRow = rows[0]?.[0] ?? 0
  return cols.filter(c => {
    const a = ctx.idx.get([...segs, firstRow, c])
    return !a?.hidden && !ctx.hidden.has(toPointer([...segs, firstRow, c]))
  })
}

/** Position columns are replaced by an "at waypoint" picker. */
const POS = new Set(['x', 'y', 'lat', 'lon'])

function PositionPicker({ ctx, segs }: { ctx: Ctx; segs: Seg[] }) {
  const row = getAt(ctx.value, segs) as JsonObject
  const x = row.x as number
  const y = row.y as number
  const at = ctx.points.findIndex(p => Math.hypot(p.x - x, p.y - y) < 50)
  return (
    <select className="vs-input" style={{ ...inputStyle, fontFamily: 'inherit', width: '100%' }} disabled={ctx.readOnly} value={at}
      title="Where this point is: pick one of your route's waypoints"
      onChange={e => {
        const p = ctx.points[Number(e.target.value)]
        if (!p) return
        const next: JsonObject = { ...row, x: p.x, y: p.y }
        if ('lat' in row) next.lat = p.lat
        if ('lon' in row) next.lon = p.lon
        ctx.set(segs, next)
      }}>
      <option value={-1}>{x === 0 && y === 0 ? 'Pick a waypoint…' : 'Custom position'}</option>
      {ctx.points.map((p, i) => <option key={i} value={i}>{p.label}</option>)}
    </select>
  )
}

function RecordTable({ ctx, segs }: { ctx: Ctx; segs: Seg[] }) {
  const v = getAt(ctx.value, segs) as Json
  const a = ctx.idx.get(segs)
  const rows = entriesOf(v, a)
  const isArr = Array.isArray(v)
  const cols = columnsOf(ctx, segs, rows, a?.item)
  const hasPos = cols.includes('x') && cols.includes('y')
  const shown = cols.filter(c => !(hasPos && POS.has(c)))
  const defLen = (getAt(ctx.defaults, segs) as Json[] | undefined)?.length ?? 0
  const canAdd = isArr && !ctx.readOnly && !a?.readonly && a?.fixedLength == null && (a?.maxItems == null || rows.length < a.maxItems)
  const canRemove = isArr && !ctx.readOnly && !a?.readonly && a?.fixedLength == null && (a?.minItems == null || rows.length > a.minItems)
  const firstRow = rows[0]?.[0] ?? 0
  const rowWords = rows.length ? [rowLabel(ctx, segs, rows[0][0], rows[0][1])] : []

  return (
    <div>
      <div style={{ overflowX: 'auto', maxHeight: rows.length > 14 ? 420 : undefined, overflowY: rows.length > 14 ? 'auto' : undefined, border: '1px solid var(--border)', borderRadius: 4 }}>
        <table style={{ borderCollapse: 'collapse', width: '100%', fontSize: '0.7rem' }}>
          <thead>
            <tr>
              <th style={th} />
              {hasPos && <th style={th}>Position</th>}
              {shown.map(c => {
                const ca = ctx.idx.get([...segs, firstRow, c])
                const unit = ca?.display?.unit ?? ca?.unit
                return (
                  <th key={c} style={th} title={ca?.help}>
                    {shortLabel(ca?.label ?? humanise(c), rowWords)}{unit ? ` (${unit})` : ''}
                  </th>
                )
              })}
              {canRemove && <th style={th} />}
            </tr>
          </thead>
          <tbody>
            {rows.length === 0 && (
              <tr><td style={{ ...td, color: 'var(--text-dim)', padding: 10 }} colSpan={shown.length + 3}>None yet.</td></tr>
            )}
            {rows.map(([k, x]) => {
              const rs = [...segs, k]
              return (
                <tr key={String(k)} style={{ background: changesUnder(ctx, rs) ? 'rgba(106,171,31,0.07)' : undefined }}>
                  <td style={{ ...td, fontWeight: 600, whiteSpace: 'nowrap', color: 'var(--text)', paddingLeft: 8 }}>{rowLabel(ctx, segs, k, x)}</td>
                  {hasPos && <td style={{ ...td, minWidth: 150 }}><PositionPicker ctx={ctx} segs={rs} /></td>}
                  {shown.map(c => (
                    <td key={c} style={{ ...td, minWidth: isScalarBool(x, c) ? 0 : 70, textAlign: isScalarBool(x, c) ? 'center' : undefined }}>
                      {(x as JsonObject)[c] !== undefined && <Control ctx={ctx} segs={[...rs, c]} compact />}
                    </td>
                  ))}
                  {canRemove && (
                    <td style={td}>
                      {(typeof k === 'number' && k >= defLen) || a?.minItems != null || !defLen ? (
                        <button type="button" className="vs-btn-ghost" style={{ padding: 2, lineHeight: 0 }} title="Remove"
                          onClick={() => ctx.set(segs, (v as Json[]).filter((_, j) => j !== k))}><Trash2 size={11} /></button>
                      ) : null}
                    </td>
                  )}
                </tr>
              )
            })}
          </tbody>
        </table>
      </div>
      {canAdd && (
        <button type="button" className="vs-btn-ghost" style={{ ...inputStyle, display: 'inline-flex', gap: 4, alignItems: 'center', marginTop: 6, fontFamily: 'inherit' }}
          onClick={() => {
            const arr = v as Json[]
            const tmpl = a?.item !== undefined ? clone(a.item) : arr.length ? clone(arr[arr.length - 1]) : {}
            // Number the new row on from the last where the template counts.
            if (isObj(tmpl)) {
              for (const f of ['num', 'number', 'Number', 'wypt_num']) {
                if (typeof tmpl[f] === 'number') {
                  const last = arr.length ? ((arr[arr.length - 1] as JsonObject)[f] as number) : (tmpl[f] as number) - 1
                  tmpl[f] = (typeof last === 'number' ? last : 0) + 1
                }
              }
            }
            ctx.set(segs, [...arr, tmpl])
          }}>
          <Plus size={11} /> Add
        </button>
      )}
    </div>
  )
}

const isScalarBool = (row: Json, c: string) => isObj(row) && typeof row[c] === 'boolean'

/** A small matrix card: the object children are columns (Chaff, Flare...),
 *  their shared keys rows (burst quantity, burst interval...), the way the
 *  cockpit lays out a CMDS program. */
function MatrixCard({ ctx, segs, title }: { ctx: Ctx; segs: Seg[]; title?: string }) {
  const v = getAt(ctx.value, segs) as JsonObject
  const groups = Object.keys(v).filter(k => !isScalar(v[k]))
  const scalars = Object.keys(v).filter(k => isScalar(v[k]) && !ctx.idx.get([...segs, k])?.hidden && !ctx.hidden.has(toPointer([...segs, k])))
  const keys = Object.keys(v[groups[0]] as JsonObject).filter(c => !ctx.idx.get([...segs, groups[0], c])?.hidden)
  const changed = changesUnder(ctx, segs) > 0
  const first = labelOf(ctx, [...segs, groups[0]])
  return (
    <div style={{ border: `1px solid ${changed ? 'var(--accent)' : 'var(--border)'}`, borderRadius: 6, padding: 8, background: 'var(--bg-elevated)', minWidth: 0 }}>
      {title && (
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 4 }}>
          <span style={{ fontWeight: 700, fontSize: '0.76rem' }}>{title}</span>
          <ResetButton ctx={ctx} segs={segs} />
        </div>
      )}
      {scalars.map(k => <FieldRow key={k} ctx={ctx} segs={[...segs, k]} />)}
      <table style={{ borderCollapse: 'collapse', width: '100%', fontSize: '0.68rem', tableLayout: 'fixed' }}>
        <thead>
          <tr>
            <th style={{ ...th, position: 'static', background: 'transparent', width: '38%' }} />
            {groups.map(g => <th key={g} style={{ ...th, position: 'static', background: 'transparent', textAlign: 'center' }}>{labelOf(ctx, [...segs, g])}</th>)}
          </tr>
        </thead>
        <tbody>
          {keys.map(k => {
            const ka = ctx.idx.get([...segs, groups[0], k])
            const unit = ka?.display?.unit ?? ka?.unit
            return (
              <tr key={k}>
                <td style={{ ...td, paddingRight: 6, color: 'var(--text-dim)' }} title={ka?.help}>
                  {shortLabel(ka?.label ?? humanise(k), [first, groups[0]])}{unit ? ` (${unit})` : ''}
                </td>
                {groups.map(g => <td key={g} style={td}><Control ctx={ctx} segs={[...segs, g, k]} compact /></td>)}
              </tr>
            )
          })}
        </tbody>
      </table>
    </div>
  )
}

function MatrixGrid({ ctx, segs }: { ctx: Ctx; segs: Seg[] }) {
  const v = getAt(ctx.value, segs) as Json
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(340px, 1fr))', gap: 10 }}>
      {entriesOf(v, ctx.idx.get(segs)).map(([k, x]) => (
        <MatrixCard key={String(k)} ctx={ctx} segs={[...segs, k]} title={rowLabel(ctx, segs, k, x)} />
      ))}
    </div>
  )
}

function Section({ title, help, changed, defaultOpen, children, right }: {
  title: string; help?: string; changed: number; defaultOpen: boolean; children: ReactNode; right?: ReactNode
}) {
  const [open, setOpen] = useState(defaultOpen)
  return (
    <div style={{ border: '1px solid var(--border)', borderRadius: 6, margin: '8px 0', background: 'var(--bg-card)' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 8, padding: '6px 8px' }}>
        <button type="button" onClick={() => setOpen(o => !o)} aria-expanded={open}
          style={{ display: 'flex', alignItems: 'center', gap: 6, flex: 1, background: 'none', border: 'none', color: 'var(--text)', cursor: 'pointer', padding: 0, textAlign: 'left', fontSize: '0.76rem', fontWeight: 700 }}>
          {open ? <ChevronDown size={12} /> : <ChevronRight size={12} />}
          {title}
          {changed > 0 && <span style={{ fontSize: '0.6rem', fontWeight: 600, color: 'var(--accent)' }}>{changed} changed</span>}
        </button>
        {right}
      </div>
      {open && (
        <div style={{ padding: '0 10px 10px' }}>
          {help && <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)', lineHeight: 1.5, marginBottom: 6 }}>{help}</div>}
          {children}
        </div>
      )}
    </div>
  )
}

/** A route-filled section: nothing to edit, say where it comes from. */
function RouteNote({ ctx, segs }: { ctx: Ctx; segs: Seg[] }) {
  const a = ctx.idx.get(segs)
  const what = a?.managed === 'route' ? 'Filled from your route on the map.' : a?.managed === 'terrain' ? 'Set from the map your route is on.' : 'Set from the plan name.'
  return (
    <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)', padding: '6px 0', lineHeight: 1.5 }}>
      <b style={{ color: 'var(--text)' }}>{a?.label ?? labelOf(ctx, segs)}:</b> {what}
    </div>
  )
}

/** Whatever is at `segs`, shown in the friendliest shape it fits. */
export function Body({ ctx, segs, depth = 0, scalarsOnly = false }: { ctx: Ctx; segs: Seg[]; depth?: number; scalarsOnly?: boolean }) {
  const v = getAt(ctx.value, segs) as Json | undefined
  const a = ctx.idx.get(segs)
  if (v === undefined || a?.hidden || ctx.hidden.has(toPointer(segs))) return null
  if (a?.managed && segs.length) return <RouteNote ctx={ctx} segs={segs} />
  if (isScalar(v)) {
    // A nullable section that is off.
    if (v === null && a?.nullable && isObj(a.item ?? null)) {
      return (
        <div style={{ display: 'flex', gap: 8, alignItems: 'center', fontSize: '0.7rem', color: 'var(--text-dim)', padding: '4px 0' }}>
          {labelOf(ctx, segs)}: not set
          {!ctx.readOnly && <button type="button" className="vs-btn-ghost" style={{ ...inputStyle, fontFamily: 'inherit' }} onClick={() => ctx.set(segs, clone(a.item!))}>Set it</button>}
        </div>
      )
    }
    return <FieldRow ctx={ctx} segs={segs} />
  }
  if (!scalarsOnly) {
    if (isRecordTable(v, a)) return <RecordTable ctx={ctx} segs={segs} />
    if (isMatrixList(v)) return <MatrixGrid ctx={ctx} segs={segs} />
    if (isMatrix(v)) return <MatrixCard ctx={ctx} segs={segs} />
  }
  const kids = entriesOf(v, a).filter(([k]) => {
    const ka = ctx.idx.get([...segs, k])
    return !ka?.hidden && !ctx.hidden.has(toPointer([...segs, k]))
  })
  const scalars = kids.filter(([, x]) => isScalar(x))
  const groups = scalarsOnly ? [] : kids.filter(([, x]) => !isScalar(x))
  return (
    <div>
      {scalars.map(([k]) => <Body key={String(k)} ctx={ctx} segs={[...segs, k]} depth={depth + 1} />)}
      {groups.map(([k, x]) => {
        const ks = [...segs, k]
        const ka = ctx.idx.get(ks)
        if (ka?.managed) return <RouteNote key={String(k)} ctx={ctx} segs={ks} />
        return (
          <Section key={String(k)} title={typeof k === 'string' ? labelOf(ctx, ks) : rowLabel(ctx, segs, k, x)} help={ka?.help} changed={changesUnder(ctx, ks)}
            defaultOpen={groups.length <= 3 || depth === 0 && groups.length <= 6}
            right={<ResetButton ctx={ctx} segs={ks} />}>
            <Body ctx={ctx} segs={ks} depth={depth + 1} />
          </Section>
        )
      })}
    </div>
  )
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

export function HitRow({ ctx, hit }: { ctx: Ctx; hit: LeafHit }) {
  return (
    <div style={{ display: 'grid', gridTemplateColumns: 'minmax(160px, 50%) 1fr', gap: 8, alignItems: 'center', padding: '4px 0', borderBottom: '1px solid var(--border)' }}>
      <span style={{ fontSize: '0.68rem', lineHeight: 1.35 }}>
        <span style={{ color: 'var(--text-dim)' }}>{hit.path.slice(0, -1).join(' › ')}{hit.path.length > 1 ? ' › ' : ''}</span>
        <span style={{ color: changesUnder(ctx, hit.segs) ? 'var(--accent)' : 'var(--text)' }}>{hit.path[hit.path.length - 1]}</span>
      </span>
      <span style={{ display: 'flex', gap: 6, alignItems: 'center', minWidth: 0 }}>
        <Control ctx={ctx} segs={hit.segs} />
        <ResetButton ctx={ctx} segs={hit.segs} />
      </span>
    </div>
  )
}

export { isRecordTable, isMatrix, isMatrixList }
