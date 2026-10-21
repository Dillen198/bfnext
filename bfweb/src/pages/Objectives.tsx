import React from 'react'
import { useQuery } from '@tanstack/react-query'
import QueryState from '../components/QueryState'
import { useState, useMemo } from 'react'
import { api, type NavaidEntry, type CaptureEvent, type Frontlines } from '../api'
import HealthBar from '../components/HealthBar'
import SideBadge from '../components/SideBadge'
import PageHeader from '../components/PageHeader'
import TacMap from '../components/TacMap'
import { ago } from '../lib/format'
import { attentionList, TONE } from '../lib/objectives'
import { useRound } from '../context/RoundContext'
import { Objective, Alert, Pin, Capture } from '@icons'
import CaptureTimeline from '../components/CaptureTimeline'
import Panel from '../components/Panel'
import ObjectiveDrawer from '../components/ObjectiveDrawer'

type Filter = 'All' | 'Red' | 'Blue' | 'Neutral'
type SortKey = 'name' | 'kind' | 'owner' | 'health' | 'logi' | 'supply' | 'fuel' | 'navaid' | 'last_change'

// Table columns, in order. `key` set = header is a clickable sort control.
const COLS: { label: string; key?: SortKey }[] = [
  { label: 'Name', key: 'name' },
  { label: 'Type', key: 'kind' },
  { label: 'Owner', key: 'owner' },
  { label: 'Health', key: 'health' },
  { label: 'Logistics', key: 'logi' },
  { label: 'Supply', key: 'supply' },
  { label: 'Fuel', key: 'fuel' },
  { label: 'Navaids', key: 'navaid' },
  { label: 'Last Taken' },
  { label: 'Last Change', key: 'last_change' },
]

/** Compact navaid string for one entry, e.g. "74Y KUT · NDB 375 · ICLS 3". */
function navaidSummary(n: NavaidEntry): string {
  const parts: string[] = []
  if (n.tacan) parts.push(n.tacan)
  if (n.ndb_khz) parts.push(`NDB ${n.ndb_khz}`)
  if (n.icls) parts.push(`ICLS ${n.icls}`)
  if (n.link4_mhz) parts.push(`L4 ${n.link4_mhz.toFixed(1)}`)
  if (n.acls) parts.push('ACLS')
  if (n.brc != null) parts.push(`BRC ${String(n.brc).padStart(3, '0')}`)
  return parts.join(' · ')
}

/** Summary for an objective's whole navaid set (carriers have one per ship). */
function navaidCell(entries: NavaidEntry[]): string {
  return entries
    .map(n => (n.deck ? `${n.deck}: ${navaidSummary(n)}` : navaidSummary(n)))
    .join('   |   ')
}

const OBJ_KINDS = ['Airbase', 'FARP', 'FOB', 'Factory', 'Logistics Hub', 'Naval Base', 'Carrier Group', 'Command Center']
const KIND_ICONS: Record<string, string> = {
  Airbase: '✈', FARP: '⬡', FOB: '▲', Factory: '⚙',
  'Logistics Hub': '◈', 'Naval Base': '⚓', 'Carrier Group': '⚓', 'Command Center': '◆',
}
const KIND_COLORS: Record<string, string> = {
  Airbase: '#3b82f6', FARP: '#22c55e', FOB: '#f97316', Factory: '#a78bfa',
  'Logistics Hub': '#06b6d4', 'Naval Base': '#0ea5e9', 'Carrier Group': '#0ea5e9', 'Command Center': '#eab308',
}

// Health and Logistics are defense-strength stats (% of a unit group still
// alive); Supply and Fuel are warehouse stock levels (% of inventory still
// stocked). They're easy to conflate -- Logistics tracks whether the base's
// logistics defenders are alive, not whether it has fuel/supplies on hand.
const COLUMN_HELP: Partial<Record<string, string>> = {
  Health: 'Overall % of this objective\'s defending units still alive (all unit types combined).',
  Logistics: '% of the logistics-defense unit group still alive, reduced further if warehouse buildings are destroyed. Governs repair speed and capturability -- not a fuel/resource level.',
  Supply: '% fill of the warehouse\'s equipment stock (weapons, vehicles, aircraft) -- a resource level, not defender health.',
  Fuel: '% fill of the warehouse\'s fuel stock -- a resource level, not defender health.',
  'Last Taken': 'The most recent capture this round: by which side, and who flew it.',
}

const SIDE_COLOR: Record<string, string> = { Blue: 'var(--blue)', Red: 'var(--red)', Neutral: '#64748b' }



function LastTaken({ c, now }: { c: CaptureEvent | undefined; now: number }) {
  if (!c) return <span style={{ color: 'var(--text-dim)' }}>—</span>
  // An all-zero UCID is bfdb's "no pilot credited".
  const by = c.by.filter(n => !/^0+$/.test(n))
  return (
    <span title={new Date(c.time).toLocaleString()}>
      <span style={{ color: SIDE_COLOR[c.side] }}>{c.side}</span>
      {by.length > 0 && <span> · {by.join(', ')}</span>}
      <span style={{ color: 'var(--text-dim)' }}> · {ago(c.time, now)}</span>
    </span>
  )
}

export default function Objectives() {
  const { selectedRound } = useRound()
  const { data: objectives = [], isLoading, isError, error, refetch } = useQuery({
    queryKey: ['objectives', selectedRound],
    queryFn: () => api.objectives(selectedRound),
    refetchInterval: 30_000,
  })
  // Navaids come from the per-side briefing; fetch both so every owned
  // objective gets its entry regardless of side. Only on the active round.
  const isActiveRound = selectedRound == null
  const { data: blueBrief } = useQuery({
    queryKey: ['briefing', 'Blue'], queryFn: () => api.briefing('Blue'),
    refetchInterval: 60_000, enabled: isActiveRound, retry: false,
  })
  const { data: redBrief } = useQuery({
    queryKey: ['briefing', 'Red'], queryFn: () => api.briefing('Red'),
    refetchInterval: 60_000, enabled: isActiveRound, retry: false,
  })
  // A carrier task force yields several entries (one per ship); ground bases one.
  const navaidByName = useMemo(() => {
    const m = new Map<string, NavaidEntry[]>()
    for (const n of [...(blueBrief?.navaids ?? []), ...(redBrief?.navaids ?? [])]) {
      const arr = m.get(n.objective) ?? []
      arr.push(n)
      m.set(n.objective, arr)
    }
    return m
  }, [blueBrief, redBrief])

  // The round's capture history. Cheap (a bounded sled scan, no engine RPC),
  // so it can follow the round selector rather than being pinned to live.
  const { data: captureEvents = [] } = useQuery({
    queryKey: ['capture-events', selectedRound],
    queryFn: () => api.captureEvents(selectedRound, 200),
    refetchInterval: 60_000,
  })

  // Clicking a row opens the detail drawer. Kept in the URL so a specific
  // objective can be linked to, same as the round selector.
  const [drawer, setDrawer] = useState<string | null>(null)

  const [filter, setFilter] = useState<Filter>('All')
  const [kindFilter, setKindFilter] = useState<string | null>(null)
  const [search, setSearch] = useState('')
  const [sortBy, setSortBy] = useState<SortKey>('health')
  const [sortDir, setSortDir] = useState<'asc' | 'desc'>('desc')

  function toggleSort(k: SortKey) {
    if (k === sortBy) {
      setSortDir(d => (d === 'asc' ? 'desc' : 'asc'))
    } else {
      setSortBy(k)
      // text columns default A→Z, numeric/date columns default high→low
      setSortDir(k === 'name' || k === 'kind' || k === 'owner' ? 'asc' : 'desc')
    }
  }

  const counts = useMemo(() => ({
    Red:     objectives.filter(o => o.owner === 'Red').length,
    Blue:    objectives.filter(o => o.owner === 'Blue').length,
    Neutral: objectives.filter(o => o.owner === 'Neutral').length,
  }), [objectives])

  const total = objectives.length

  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({
    queryKey: ['frontline', selectedRound], queryFn: () => api.frontline(selectedRound), refetchInterval: 30_000,
  })

  // Newest capture per objective (events arrive newest first).
  const lastCapture = useMemo(() => {
    const m = new Map<string, CaptureEvent>()
    for (const e of captureEvents) if (!m.has(e.objective)) m.set(e.objective, e)
    return m
  }, [captureEvents])

  const [now] = useState(() => Date.now())
  const attention = useMemo(() => attentionList(objectives, captureEvents, now), [objectives, captureEvents, now])

  // Who holds what kind of base.
  const byKind = OBJ_KINDS.map(k => {
    const of = objectives.filter(o => o.kind === k)
    return {
      kind: k, total: of.length,
      Blue: of.filter(o => o.owner === 'Blue').length,
      Red: of.filter(o => o.owner === 'Red').length,
      Neutral: of.filter(o => o.owner === 'Neutral').length,
    }
  }).filter(k => k.total > 0)
  const kindMax = Math.max(1, ...byKind.map(k => k.total))

  const filtered = useMemo(() => objectives
    .filter(o => {
      if (filter !== 'All' && o.owner !== filter) return false
      if (kindFilter && o.kind !== kindFilter) return false
      if (search && !o.name.toLowerCase().includes(search.toLowerCase())) return false
      return true
    })
    .sort((a, b) => {
      const dir = sortDir === 'asc' ? 1 : -1
      switch (sortBy) {
        case 'name':  return dir * a.name.localeCompare(b.name)
        case 'kind':  return dir * a.kind.localeCompare(b.kind)
        case 'owner': return dir * a.owner.localeCompare(b.owner)
        case 'navaid': {
          const na = navaidByName.get(a.name) ? navaidCell(navaidByName.get(a.name)!) : ''
          const nb = navaidByName.get(b.name) ? navaidCell(navaidByName.get(b.name)!) : ''
          return dir * na.localeCompare(nb)
        }
        case 'last_change':
          return dir * (new Date(a.last_change).getTime() - new Date(b.last_change).getTime())
        default:
          return dir * ((a[sortBy] as number) - (b[sortBy] as number))
      }
    }),
    [objectives, filter, kindFilter, search, sortBy, sortDir, navaidByName]
  )

  const bluePct    = total > 0 ? counts.Blue    / total * 100 : 0
  const redPct     = total > 0 ? counts.Red     / total * 100 : 0
  const neutralPct = total > 0 ? counts.Neutral / total * 100 : 0

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="tank"
        icon={Objective}
        title="OBJECTIVES"
        sub={`${total} total · ${counts.Red} red · ${counts.Blue} blue · ${counts.Neutral} neutral`}
        right={
          <input
            type="text"
            placeholder="Search…"
            value={search}
            onChange={e => setSearch(e.target.value)}
            className="vs-input w-36"
          />
        }
      />

      <div className="flex-1 overflow-auto vs-page" style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>

        {/* ── Territory bar ── */}
        {total > 0 && (
          <div className="vs-card px-5 py-4">
            <div className="flex items-center justify-between mb-2">
              <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.12em', display: 'flex', alignItems: 'center', gap: 6 }}>
                <Pin size={11} style={{ color: 'var(--text-dim)' }} />
                Territory Control
              </span>
              <div style={{ display: 'flex', alignItems: 'center', gap: 16, fontSize: '0.65rem', fontFamily: 'var(--font-mono)' }}>
                <span style={{ color: 'var(--blue)' }}>{Math.round(bluePct)}% Blue</span>
                <span style={{ color: 'var(--border-light)' }}>·</span>
                <span style={{ color: 'var(--red)' }}>{Math.round(redPct)}% Red</span>
                <span style={{ color: 'var(--border-light)' }}>·</span>
                <span style={{ color: 'var(--text-dim)' }}>{Math.round(neutralPct)}% Neutral</span>
              </div>
            </div>
            <div style={{ height: 8, overflow: 'hidden', display: 'flex', background: 'var(--bg-elevated)', borderRadius: 2 }}>
              <div className="health-fill" style={{ width: `${bluePct}%`, height: '100%', background: 'var(--blue)' }} />
              <div className="health-fill" style={{ width: `${neutralPct}%`, height: '100%', background: 'var(--border-light)' }} />
              <div className="health-fill" style={{ width: `${redPct}%`, height: '100%', background: 'var(--red)' }} />
            </div>
          </div>
        )}

        {/* ── Map + needs attention ── */}
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: 12 }}>
          <div className="vs-card p-0" style={{ flex: '3 1 420px', minWidth: 0, height: 400, overflow: 'hidden' }}>
            <TacMap objectives={objectives} fronts={fronts} onSelect={setDrawer} />
          </div>
          <div style={{ flex: '2 1 300px', minWidth: 0, height: 400, display: 'flex', flexDirection: 'column' }}>
            <Panel title="Needs Attention" icon={Alert} iconColor="#ca8a04"
              right={<span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>{attention.length}</span>}>
              <div style={{ overflowY: 'auto', maxHeight: 352 }}>
                {attention.length === 0 ? (
                  <div style={{ padding: 20, fontSize: '0.7rem', color: 'var(--text-dim)' }}>
                    Nothing is under attack, about to fall, low on stock or freshly taken.
                  </div>
                ) : attention.map(({ obj, reasons }) => (
                  <button key={obj.id} type="button" onClick={() => setDrawer(obj.name)}
                    style={{ all: 'unset', boxSizing: 'border-box', width: '100%', cursor: 'pointer', display: 'flex', gap: 10, alignItems: 'center', padding: '9px 14px', borderBottom: '1px solid var(--border)' }}>
                    <span style={{ color: SIDE_COLOR[obj.owner], fontSize: '0.95rem', width: 16, textAlign: 'center', flexShrink: 0 }}>{KIND_ICONS[obj.kind] ?? '■'}</span>
                    <span style={{ minWidth: 0, flex: 1 }}>
                      <span className="flex items-center gap-2">
                        <span style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{obj.name}</span>
                        <SideBadge side={obj.owner} size="xs" />
                      </span>
                      <span className="flex flex-wrap" style={{ gap: 4, marginTop: 4 }}>
                        {reasons.map(r => (
                          <span key={r.text} style={{ fontSize: '0.58rem', letterSpacing: '0.04em', padding: '1px 6px', borderRadius: 2, color: TONE[r.tone].fg, background: TONE[r.tone].bg }}>{r.text}</span>
                        ))}
                      </span>
                    </span>
                  </button>
                ))}
              </div>
            </Panel>
          </div>
        </div>

        {/* ── Who holds what ── */}
        {byKind.length > 0 && (
          <Panel title="Holdings by Type" icon={Pin} iconColor="var(--accent)"
            right={
              <span className="flex items-center gap-3" style={{ fontSize: '0.6rem', color: 'var(--text-muted)' }}>
                {(['Blue', 'Red', 'Neutral'] as const).map(sd => (
                  <span key={sd} className="flex items-center gap-1.5"><span style={{ width: 8, height: 8, borderRadius: 2, background: SIDE_COLOR[sd], display: 'inline-block' }} />{sd}</span>
                ))}
              </span>
            }>
            <div style={{ padding: '12px 20px', display: 'grid', gridTemplateColumns: 'minmax(100px, 150px) 1fr auto', gap: '8px 12px', alignItems: 'center' }}>
              {byKind.map(k => (
                <React.Fragment key={k.kind}>
                  <button type="button" onClick={() => setKindFilter(kindFilter === k.kind ? null : k.kind)}
                    title={kindFilter === k.kind ? 'Show every type' : `Show only ${k.kind} in the table`}
                    style={{ all: 'unset', cursor: 'pointer', fontSize: '0.7rem', color: kindFilter === k.kind ? 'var(--accent-bright)' : 'var(--text)' }}>
                    <span style={{ color: 'var(--text-dim)', marginRight: 6 }}>{KIND_ICONS[k.kind] ?? '■'}</span>{k.kind}
                  </button>
                  <div style={{ display: 'flex', gap: 2, height: 10, width: `${(k.total / kindMax) * 100}%`, minWidth: 6 }}
                    title={`${k.kind}: ${k.Blue} Blue · ${k.Red} Red · ${k.Neutral} neutral`}>
                    {(['Blue', 'Red', 'Neutral'] as const).filter(sd => k[sd] > 0).map((sd, i, arr) => (
                      <div key={sd} style={{
                        flex: k[sd], background: SIDE_COLOR[sd], minWidth: 2,
                        borderRadius: `${i === 0 ? 2 : 0}px ${i === arr.length - 1 ? 4 : 0}px ${i === arr.length - 1 ? 4 : 0}px ${i === 0 ? 2 : 0}px`,
                      }} />
                    ))}
                  </div>
                  <span className="font-mono-vs" style={{ fontSize: '0.64rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>
                    {k.Blue} / {k.Red}{k.Neutral ? ` / ${k.Neutral}` : ''}
                  </span>
                </React.Fragment>
              ))}
            </div>
          </Panel>
        )}

        {/* Capture timeline -- full width. It carries a chart AND a scrolling
            event list, so squeezed into a third of the row it was twice the
            height of its neighbours and stretched the whole grid. */}
        <Panel title="Capture Timeline" icon={Capture} iconColor="#fbbf24">
          <CaptureTimeline events={captureEvents} />
        </Panel>

        {/* ── Controls ── */}
        <div className="flex items-center gap-2 flex-wrap">
          <div className="flex overflow-hidden" style={{ border: '1px solid var(--border)', borderRadius: '2px' }}>
            {(['All', 'Red', 'Blue', 'Neutral'] as Filter[]).map(f => (
              <button
                key={f}
                className="vs-pill"
                onClick={() => setFilter(f)}
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '0.75rem',
                  letterSpacing: '0.1em',
                  padding: '0.3rem 0.75rem',
                  background: filter === f ? 'var(--accent-glow)' : 'transparent',
                  color: filter === f ? 'var(--accent)' : 'var(--text-muted)',
                  border: 'none',
                  cursor: 'pointer',
                  transition: 'all 0.15s',
                }}
              >
                {f.toUpperCase()}
              </button>
            ))}
          </div>
          {kindFilter && (
            <button type="button" onClick={() => setKindFilter(null)} className="vs-pill"
              style={{ fontSize: '0.62rem', padding: '0.25rem 0.6rem', border: '1px solid var(--accent-border)', background: 'var(--accent-glow)', color: 'var(--accent-bright)', cursor: 'pointer', borderRadius: 2 }}>
              {kindFilter} ✕
            </button>
          )}
          <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', marginLeft: '0.25rem' }}
            title="Health and Logistics are defender strength (% of units alive). Supply and Fuel are warehouse stock levels.">
            Health/Logistics = defenders alive · Supply/Fuel = stock · hover a header for more
          </span>
          <span className="ml-auto font-mono-vs" style={{ fontSize: '0.65rem', color: 'var(--text-dim)' }}>{filtered.length} shown</span>
        </div>

        {/* ── Table ── */}
        <div className="vs-card overflow-hidden" style={{ display: 'flex', flexDirection: 'column', flex: '1 1 0', minHeight: 200 }}>
          <div style={{ overflow: 'auto', flex: '1 1 auto', minHeight: 0 }}>
            <table className="w-full">
              <thead style={{ background: 'var(--bg-card)', borderBottom: '1px solid var(--border)', position: 'sticky', top: 0, zIndex: 1 }}>
                <tr>
                  {COLS.map(({ label, key }) => {
                    const active = key && sortBy === key
                    return (
                      <th
                        key={label}
                        title={COLUMN_HELP[label]}
                        onClick={key ? () => toggleSort(key) : undefined}
                        style={{
                          padding: '10px 14px', textAlign: 'left', fontSize: '0.62rem',
                          textTransform: 'uppercase', letterSpacing: '0.1em',
                          color: active ? 'var(--accent)' : 'var(--text-dim)',
                          fontWeight: 700, whiteSpace: 'nowrap', userSelect: 'none',
                          cursor: key ? 'pointer' : (COLUMN_HELP[label] ? 'help' : undefined),
                        }}
                      >
                        {label}
                        <span style={{ marginLeft: 4, opacity: active ? 1 : 0.25 }}>
                          {active ? (sortDir === 'asc' ? '▲' : '▼') : (key ? '↕' : '')}
                        </span>
                      </th>
                    )
                  })}
                </tr>
              </thead>
              <tbody>
                <QueryState
                  colSpan={10} what="objectives" isLoading={isLoading}
                  isError={isError} error={error} onRetry={refetch}
                />
                {filtered.map(obj => {
                  const isCrit = obj.health < 40
                  return (
                    <tr
                      key={obj.id}
                      className="kill-row"
                      onClick={() => setDrawer(obj.name)}
                      style={{ borderBottom: '1px solid var(--border)', background: isCrit ? 'rgba(239,68,68,0.02)' : 'transparent', cursor: 'pointer' }}
                    >
                      <td style={{ padding: '9px 14px' }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
                          <span style={{ color: KIND_COLORS[obj.kind] ?? 'var(--text-dim)' }}>{KIND_ICONS[obj.kind] ?? '■'}</span>
                          <span style={{ fontSize: '0.82rem', fontWeight: 600, color: 'var(--text)' }}>{obj.name}</span>
                          {isCrit && <Alert size={11} style={{ color: '#f59e0b', flexShrink: 0 }} />}
                        </div>
                      </td>
                      <td style={{ padding: '9px 14px', fontSize: '0.7rem', color: 'var(--text-dim)' }}>{obj.kind}</td>
                      <td style={{ padding: '9px 14px' }}><SideBadge side={obj.owner} size="xs" /></td>
                      <td style={{ padding: '9px 14px', width: 120 }}><HealthBar value={obj.health} /></td>
                      <td style={{ padding: '9px 14px', width: 120 }}><HealthBar value={obj.logi} /></td>
                      <td style={{ padding: '9px 14px', width: 120 }}><HealthBar value={obj.supply} /></td>
                      <td style={{ padding: '9px 14px', width: 120 }}><HealthBar value={obj.fuel} /></td>
                      <td className="font-mono-vs" style={{ padding: '9px 14px', fontSize: '0.62rem', color: '#facc15', whiteSpace: 'nowrap' }}>
                        {navaidByName.has(obj.name) ? navaidCell(navaidByName.get(obj.name)!) : <span style={{ color: 'var(--text-dim)' }}>—</span>}
                      </td>
                      <td style={{ padding: '9px 14px', fontSize: '0.64rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>
                        <LastTaken c={lastCapture.get(obj.name)} now={now} />
                      </td>
                      <td className="font-mono-vs" style={{ padding: '9px 14px', fontSize: '0.62rem', color: 'var(--text-dim)', whiteSpace: 'nowrap' }}>
                        {new Date(obj.last_change).toLocaleString()}
                      </td>
                    </tr>
                  )
                })}
                <QueryState
                  colSpan={10} what="objectives" isLoading={false}
                  isEmpty={!isLoading && !isError && filtered.length === 0}
                  emptyText={objectives.length === 0
                    ? 'No objectives in this round'
                    : 'No objectives match the current filter'}
                />
              </tbody>
            </table>
          </div>
        </div>
      </div>

      <ObjectiveDrawer
        objective={drawer}
        onClose={() => setDrawer(null)}
        onOpen={setDrawer}
      />
    </div>
  )
}
