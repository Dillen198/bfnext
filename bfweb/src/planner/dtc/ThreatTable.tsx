// The RWR threat tables as a pilot thinks of them: a ranked list, most
// important first, grouped by kind, with the cockpit switches beside each.

import { useMemo, useState, type CSSProperties } from 'react'
import { ArrowDown, ArrowUp, ChevronsDown, Menu, Search } from '@icons'
import { getAt, toPointer, type Json, type JsonObject, type Seg } from '../model'
import { changesUnder, labelOf, playerHelp, type Ctx } from './core'
import { liftToTop, moveTo, renumber, SWITCH_LABELS, THREAT_KINDS, threatRows, type ThreatKind, type ThreatRow } from './threats'

const KIND_COLOUR: Record<ThreatKind, string> = {
  SAM: '#f04747', AAA: '#f0a030', Aircraft: '#40a6ff', Missiles: '#c77dff', 'EWR & AWACS': '#2ec4b6', Ships: '#7cc4ff', Other: '#9aa3ad',
}

const btn: CSSProperties = { padding: '2px 5px', fontSize: '0.62rem', lineHeight: 1 }
const chip = (on: boolean): CSSProperties => ({
  padding: '3px 9px', borderRadius: 12, fontSize: '0.64rem', cursor: 'pointer',
  border: `1px solid ${on ? 'var(--accent)' : 'var(--border)'}`, background: on ? 'var(--accent-glow, rgba(106,171,31,0.15))' : 'transparent', color: 'var(--text)',
})

/** Apply several changes in one go (a reorder touches many priorities). */
function setAll(ctx: Ctx, pairs: [Seg[], Json][]) {
  if (!pairs.length) return
  if (ctx.setMany) ctx.setMany(pairs)
  else for (const [s, v] of pairs) ctx.set(s, v)
}

/** One threat table: ranked, filterable, drag or arrow to reorder. */
export function ThreatTable({ ctx, segs }: { ctx: Ctx; segs: Seg[] }) {
  const v = getAt(ctx.value, segs) as Json
  const a = ctx.idx.get(segs)
  const rows = useMemo(() => threatRows(v, a), [v, a])
  const [query, setQuery] = useState('')
  const [kind, setKind] = useState<ThreatKind | 'all' | 'hidden'>('all')
  const [drag, setDrag] = useState<string | null>(null)

  const first = rows[0]
  const fieldAnn = (f: string) => (first ? ctx.idx.get([...segs, first.key, f]) : undefined)
  // PRI read-only (the Apache's lists merge by position): switches only.
  const canMove = !ctx.readOnly && !fieldAnn('PRI')?.readonly
  const switches = first
    ? Object.keys(first.rec).filter(f => typeof first.rec[f] === 'boolean' && !fieldAnn(f)?.hidden && !fieldAnn(f)?.readonly)
    : []
  const hasShow = switches.includes('display')

  const q = query.trim().toLowerCase()
  const shown = rows.filter(r =>
    (kind === 'all' || (kind === 'hidden' ? r.rec.display === false : r.kind === kind)) &&
    (!q || r.name.toLowerCase().includes(q) || r.types.toLowerCase().includes(q)))
  const filtered = shown.length !== rows.length
  const counts = new Map<ThreatKind, number>()
  for (const r of rows) counts.set(r.kind, (counts.get(r.kind) ?? 0) + 1)
  const hiddenCount = rows.filter(r => r.rec.display === false).length

  const reorder = (order: Seg[]) => setAll(ctx, renumber(rows, order).map(([k, n]) => [[...segs, k, 'PRI'], n]))
  const flip = (list: ThreatRow[], f: string, on: boolean) =>
    setAll(ctx, list.filter(r => r.rec[f] !== on).map(r => [[...segs, r.key, f], on]))
  const indexOf = (r: ThreatRow) => rows.findIndex(x => String(x.key) === String(r.key))
  // Up/down step over the rows on screen, so a filtered list moves as seen.
  const step = (r: ThreatRow, d: -1 | 1) => {
    const i = shown.indexOf(r)
    const other = shown[i + d]
    if (other) reorder(moveTo(rows, r.key, indexOf(other)))
  }

  return (
    <div>
      <p style={{ fontSize: '0.68rem', color: 'var(--text-dim)', lineHeight: 1.55, margin: '0 0 8px' }}>
        When the scope is crowded the RWR shows the highest-ranked threats first: <b style={{ color: 'var(--text)' }}>1 is the most important</b>.
        {canMove ? ' Drag a row, use the arrows, or type a new rank.' : ' This aircraft keeps its own order: only the switches can be changed.'}
      </p>

      <div style={{ position: 'sticky', top: -16, zIndex: 2, background: 'linear-gradient(var(--bg-card), var(--bg-card)), var(--bg)', padding: '6px 0 8px', display: 'flex', flexDirection: 'column', gap: 6 }}>
        <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap', alignItems: 'center' }}>
          <label style={{ position: 'relative', flex: '1 1 200px', maxWidth: 320 }}>
            <Search size={11} style={{ position: 'absolute', left: 7, top: 7, color: 'var(--text-dim)' }} />
            <input className="vs-input" value={query} onChange={e => setQuery(e.target.value)} placeholder="Find a threat (SA-10, MiG-29, Hawk…)"
              aria-label="Find a threat" style={{ width: '100%', padding: '4px 8px 4px 24px', fontSize: '0.7rem' }} />
          </label>
          <button type="button" style={chip(kind === 'all')} onClick={() => setKind('all')}>All {rows.length}</button>
          {THREAT_KINDS.filter(k => counts.get(k)).map(k => (
            <button key={k} type="button" style={chip(kind === k)} onClick={() => setKind(kind === k ? 'all' : k)}>
              <span style={{ display: 'inline-block', width: 7, height: 7, borderRadius: 4, background: KIND_COLOUR[k], marginRight: 5 }} />
              {k} {counts.get(k)}
            </button>
          ))}
          {hasShow && hiddenCount > 0 && <button type="button" style={chip(kind === 'hidden')} onClick={() => setKind(kind === 'hidden' ? 'all' : 'hidden')}>Hidden {hiddenCount}</button>}
        </div>
        {filtered && !ctx.readOnly && shown.length > 0 && (
          <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap', alignItems: 'center', fontSize: '0.64rem', color: 'var(--text-dim)' }}>
            {shown.length} shown:
            {canMove && <button type="button" className="vs-btn-outline" style={{ ...btn, padding: '3px 8px' }} onClick={() => reorder(liftToTop(rows, shown.map(r => r.key)))}>Rank these first</button>}
            {hasShow && <button type="button" className="vs-btn-ghost" style={{ ...btn, padding: '3px 8px' }} onClick={() => flip(shown, 'display', true)}>Show all</button>}
            {hasShow && <button type="button" className="vs-btn-ghost" style={{ ...btn, padding: '3px 8px' }} onClick={() => flip(shown, 'display', false)}>Hide all</button>}
          </div>
        )}
      </div>

      <div role="table" aria-label={labelOf(ctx, segs)} style={{ border: '1px solid var(--border)', borderRadius: 6, overflow: 'hidden' }}>
        <div role="row" style={{ ...grid(switches.length), fontSize: '0.6rem', fontWeight: 700, color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.05em', background: 'var(--bg-elevated)', padding: '6px 8px' }}>
          <span role="columnheader">Rank</span>
          <span role="columnheader">Threat</span>
          {switches.map(f => (
            <span key={f} role="columnheader" title={playerHelp(fieldAnn(f)?.help)} style={{ textAlign: 'center', lineHeight: 1.2, alignSelf: 'end' }}>{SWITCH_LABELS[f] ?? fieldAnn(f)?.label ?? f}</span>
          ))}
          <span role="columnheader" />
        </div>
        {shown.length === 0 && <div style={{ padding: 12, fontSize: '0.7rem', color: 'var(--text-dim)' }}>Nothing matches.</div>}
        {shown.map((r, i) => {
          const k = String(r.key)
          const changed = changesUnder(ctx, [...segs, r.key]) > 0
          const off = r.rec.display === false
          return (
            <div key={k} role="row" draggable={canMove}
              onDragStart={e => { setDrag(k); e.dataTransfer.effectAllowed = 'move' }}
              onDragEnd={() => setDrag(null)}
              onDragOver={e => { if (drag && drag !== k) e.preventDefault() }}
              onDrop={e => {
                e.preventDefault()
                const from = rows.find(x => String(x.key) === drag)
                if (from) reorder(moveTo(rows, from.key, indexOf(r)))
                setDrag(null)
              }}
              style={{
                ...grid(switches.length), padding: '5px 8px', alignItems: 'center', borderTop: '1px solid var(--border)',
                background: drag === k ? 'var(--bg-elevated)' : i % 2 ? 'rgba(255,255,255,0.015)' : 'transparent',
                boxShadow: changed ? 'inset 3px 0 0 var(--accent)' : undefined, opacity: off ? 0.55 : 1,
              }}>
              <span role="cell" style={{ display: 'flex', alignItems: 'center', gap: 3 }}>
                {canMove && <Menu size={11} style={{ color: 'var(--text-dim)', cursor: 'grab', flex: '0 0 auto' }} aria-hidden />}
                <RankInput rank={r.pri} max={rows.length} disabled={!canMove} label={r.name}
                  onRank={n => reorder(moveTo(rows, r.key, n - 1))} />
              </span>
              <span role="cell" style={{ minWidth: 0 }}>
                <span style={{ display: 'flex', alignItems: 'center', gap: 6, minWidth: 0 }}>
                  <span title={r.kind} style={{ flex: '0 0 auto', width: 8, height: 8, borderRadius: 4, background: KIND_COLOUR[r.kind] }} />
                  <span style={{ fontSize: '0.72rem', fontWeight: 600, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{r.name}</span>
                </span>
                {r.types && <span title={r.types} style={{ display: 'block', fontSize: '0.6rem', color: 'var(--text-dim)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', paddingLeft: 14 }}>{r.types}</span>}
              </span>
              {switches.map(f => (
                <span key={f} role="cell" style={{ textAlign: 'center' }}>
                  <input type="checkbox" checked={r.rec[f] === true} disabled={ctx.readOnly} style={{ width: 15, height: 15 }}
                    aria-label={`${SWITCH_LABELS[f] ?? f}: ${r.name}`} onChange={e => ctx.set([...segs, r.key, f], e.target.checked)} />
                </span>
              ))}
              <span role="cell" style={{ display: 'flex', gap: 2, justifyContent: 'flex-end' }}>
                {canMove && <>
                  <button type="button" className="vs-btn-ghost" style={btn} title="Rank first" aria-label={`Rank ${r.name} first`} disabled={r.pri === 1} onClick={() => reorder(moveTo(rows, r.key, 0))}><ChevronsDown size={11} style={{ transform: 'rotate(180deg)' }} /></button>
                  <button type="button" className="vs-btn-ghost" style={btn} title="Up" aria-label={`Move ${r.name} up`} disabled={i === 0} onClick={() => step(r, -1)}><ArrowUp size={11} /></button>
                  <button type="button" className="vs-btn-ghost" style={btn} title="Down" aria-label={`Move ${r.name} down`} disabled={i === shown.length - 1} onClick={() => step(r, 1)}><ArrowDown size={11} /></button>
                </>}
              </span>
            </div>
          )
        })}
      </div>
    </div>
  )
}

const grid = (n: number): CSSProperties => ({
  display: 'grid', gridTemplateColumns: `68px minmax(130px, 1fr) repeat(${n}, 54px) 74px`, columnGap: 6,
})

/** A rank you can type: moves the row there on Enter or leaving the box. */
function RankInput({ rank, max, disabled, label, onRank }: { rank: number; max: number; disabled: boolean; label: string; onRank: (n: number) => void }) {
  const [text, setText] = useState<string | null>(null)
  const commit = () => {
    const n = Math.round(Number(text))
    if (text != null && Number.isFinite(n) && n >= 1 && n <= max && n !== rank) onRank(n)
    setText(null)
  }
  return (
    <input className="vs-input" type="number" min={1} max={max} disabled={disabled} aria-label={`Rank of ${label}`}
      value={text ?? rank} onChange={e => setText(e.target.value)} onBlur={commit}
      onKeyDown={e => { if (e.key === 'Enter') (e.target as HTMLInputElement).blur() }}
      style={{ width: 50, padding: '2px 4px', fontSize: '0.7rem', fontFamily: 'var(--font-mono)', textAlign: 'center' }} />
  )
}

/** Several threat tables side by side (F-16 HIGH/LOW, Hornet's five modes): tabs. */
export function ThreatTabs({ ctx, segs, keys: given }: { ctx: Ctx; segs: Seg[]; keys: Seg[] }) {
  const [tab, setTab] = useState(0)
  // In the cartridge's own order (Hornet: NORM first), not alphabetical.
  const at = (k: Seg) => {
    const i = ctx.idx.ann.fields.findIndex(f => f.path === [...segs, k].join('.'))
    return i < 0 ? Infinity : i
  }
  const keys = [...given].sort((x, y) => at(x) - at(y))
  const tabLabel = (k: Seg) => labelOf(ctx, [...segs, k]).replace(/^RWR table /i, '')
  const cur = keys[Math.min(tab, keys.length - 1)]
  const copyTo = (to: Seg) => {
    const src = getAt(ctx.value, [...segs, cur]) as JsonObject
    const dst = getAt(ctx.value, [...segs, to]) as JsonObject
    const pairs: [Seg[], Json][] = []
    for (const [k, rec] of Object.entries(src)) {
      if (!dst || !(k in dst)) continue
      for (const [f, x] of Object.entries(rec as JsonObject)) {
        if ((typeof x === 'number' || typeof x === 'boolean') && (dst[k] as JsonObject)[f] !== x) pairs.push([[...segs, to, k, f], x])
      }
    }
    setAll(ctx, pairs)
  }
  return (
    <div>
      <div role="tablist" style={{ display: 'flex', gap: 4, flexWrap: 'wrap', marginBottom: 8, borderBottom: '1px solid var(--border)' }}>
        {keys.map((k, i) => {
          const n = changesUnder(ctx, [...segs, k])
          return (
            <button key={String(k)} type="button" role="tab" aria-selected={k === cur} onClick={() => setTab(i)}
              style={{
                padding: '6px 12px', fontSize: '0.7rem', fontWeight: 700, cursor: 'pointer', background: 'transparent', color: k === cur ? 'var(--text)' : 'var(--text-dim)',
                border: 'none', borderBottom: `2px solid ${k === cur ? 'var(--accent)' : 'transparent'}`, marginBottom: -1,
              }}>
              {tabLabel(k)}{n > 0 && <span style={{ marginLeft: 6, color: 'var(--accent)' }}>{n}</span>}
            </button>
          )
        })}
      </div>
      {playerHelp(ctx.idx.get([...segs, cur])?.help) && (
        <p style={{ fontSize: '0.68rem', color: 'var(--text-dim)', margin: '0 0 6px' }}>{playerHelp(ctx.idx.get([...segs, cur])?.help)}</p>
      )}
      {!ctx.readOnly && (
        <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap', alignItems: 'center', fontSize: '0.64rem', color: 'var(--text-dim)', marginBottom: 6 }}>
          Copy this table to:
          {keys.filter(k => k !== cur).map(k => (
            <button key={String(k)} type="button" className="vs-btn-ghost" style={{ ...btn, padding: '3px 8px' }}
              onClick={() => { if (confirm(`Make ${tabLabel(k)} the same as ${tabLabel(cur)}?`)) copyTo(k) }}>
              {tabLabel(k)}
            </button>
          ))}
        </div>
      )}
      <ThreatTable key={toPointer([...segs, cur])} ctx={ctx} segs={[...segs, cur]} />
    </div>
  )
}
