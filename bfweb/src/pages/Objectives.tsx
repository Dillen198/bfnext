import { useQuery } from '@tanstack/react-query'
import QueryState from '../components/QueryState'
import { useState, useMemo } from 'react'
import { api, type NavaidEntry } from '../api'
import HealthBar from '../components/HealthBar'
import SideBadge from '../components/SideBadge'
import PageHeader from '../components/PageHeader'
import { BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell, PieChart, Pie } from 'recharts'
import { useRound } from '../context/RoundContext'
import { Objective, Alert, Shield, Pin, Capture } from '@icons'
import CaptureTimeline from '../components/CaptureTimeline'
import Panel from '../components/Panel'
import ObjectiveDrawer from '../components/ObjectiveDrawer'

const TT = {
  contentStyle: { background: 'var(--bg-elevated)', border: '1px solid var(--border-light)', borderRadius: 4, color: 'var(--text)', fontSize: 12 },
  cursor: { fill: 'rgba(56,189,248,0.04)' },
}

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

  // Health distribution
  const healthBuckets = [
    { range: '0–25%',   count: objectives.filter(o => o.health <= 25).length,                     fill: '#ef4444' },
    { range: '26–50%',  count: objectives.filter(o => o.health > 25  && o.health <= 50).length,   fill: '#f97316' },
    { range: '51–75%',  count: objectives.filter(o => o.health > 50  && o.health <= 75).length,   fill: '#eab308' },
    { range: '76–100%', count: objectives.filter(o => o.health > 75).length,                      fill: '#22c55e' },
  ]

  // Kind distribution
  const kindData = OBJ_KINDS.map(k => ({
    name: k, count: objectives.filter(o => o.kind === k).length, fill: KIND_COLORS[k] ?? '#4b5563',
  })).filter(d => d.count > 0)

  // Critical objectives (health < 40)
  const criticalObjs = useMemo(() =>
    [...objectives].filter(o => o.health < 40).sort((a, b) => a.health - b.health).slice(0, 6),
    [objectives]
  )

  const filtered = useMemo(() => objectives
    .filter(o => {
      if (filter !== 'All' && o.owner !== filter) return false
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
    [objectives, filter, search, sortBy, sortDir, navaidByName]
  )

  const bluePct    = total > 0 ? counts.Blue    / total * 100 : 0
  const redPct     = total > 0 ? counts.Red     / total * 100 : 0
  const neutralPct = total > 0 ? counts.Neutral / total * 100 : 0

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
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
                <span style={{ color: '#60a5fa' }}>{Math.round(bluePct)}% Blue</span>
                <span style={{ color: 'var(--border-light)' }}>·</span>
                <span style={{ color: '#f87171' }}>{Math.round(redPct)}% Red</span>
                <span style={{ color: 'var(--border-light)' }}>·</span>
                <span style={{ color: 'var(--text-dim)' }}>{Math.round(neutralPct)}% Neutral</span>
              </div>
            </div>
            <div style={{ height: 8, overflow: 'hidden', display: 'flex', background: 'var(--bg-elevated)', borderRadius: 2 }}>
              <div className="health-fill" style={{ width: `${bluePct}%`, height: '100%', background: 'linear-gradient(90deg,#1d4ed8,#3b82f6)' }} />
              <div className="health-fill" style={{ width: `${neutralPct}%`, height: '100%', background: 'var(--border-light)' }} />
              <div className="health-fill" style={{ width: `${redPct}%`, height: '100%', background: 'linear-gradient(90deg,#dc2626,#991b1b)' }} />
            </div>
          </div>
        )}

        {/* ── Stat legend ── */}
        <div style={{
          background: 'var(--bg-card)', border: '1px solid var(--border)',
          borderRadius: 6, padding: '10px 16px',
          display: 'flex', alignItems: 'baseline', gap: 8, flexWrap: 'wrap',
        }}>
          <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.1em', fontWeight: 600, flexShrink: 0 }}>Stats:</span>
          <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
            <b style={{ color: 'var(--text)' }}>Health</b> &amp; <b style={{ color: 'var(--text)' }}>Logistics</b> are defender strength (% of units still alive) —
            {' '}<b style={{ color: 'var(--text)' }}>Supply</b> &amp; <b style={{ color: 'var(--text)' }}>Fuel</b> are warehouse stock levels (% of inventory remaining).
            Logistics is not the same as Fuel: it tracks whether the base's logistics defenders are alive, not how much fuel is stored there.
          </span>
        </div>

        {/* ── Critical alerts ── */}
        {criticalObjs.length > 0 && (
          <Panel
            title="Critical Objectives"
            icon={Alert}
            iconColor="#fbbf24"
            right={<span style={{ fontSize: '0.6rem', color: '#f59e0b', fontFamily: 'var(--font-mono)', letterSpacing: '0.1em' }}>{criticalObjs.length} CRITICAL</span>}
          >
            <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(220px, 1fr))' }}>
              {criticalObjs.map(obj => (
                <div key={obj.id} style={{ display: 'flex', alignItems: 'center', gap: 12, padding: '12px 20px', borderBottom: '1px solid var(--border)' }}>
                  <div style={{ fontSize: '1.2rem', color: KIND_COLORS[obj.kind] ?? 'var(--text-dim)', flexShrink: 0 }}>
                    {KIND_ICONS[obj.kind] ?? '■'}
                  </div>
                  <div style={{ flex: 1, minWidth: 0 }}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: 8, marginBottom: 6 }}>
                      <span style={{ fontSize: '0.82rem', fontWeight: 600, color: 'var(--text)' }}>{obj.name}</span>
                      <SideBadge side={obj.owner} size="xs" />
                    </div>
                    <HealthBar value={obj.health} />
                    <div style={{ display: 'flex', gap: 12, marginTop: 4, fontSize: '0.6rem', fontFamily: 'var(--font-mono)', color: 'var(--text-dim)' }}>
                      <span>L:{obj.logi}%</span>
                      <span>S:{obj.supply}%</span>
                      <span>F:{obj.fuel}%</span>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </Panel>
        )}

        {/* ── Charts row ── */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-4 items-start">
          {/* Ownership bars */}
          <Panel title="Ownership" icon={Shield} iconColor="#60a5fa">
            <div style={{ padding: '16px 20px', display: 'flex', flexDirection: 'column', gap: 12 }}>
              {([['Blue', counts.Blue, '#3b82f6'], ['Red', counts.Red, '#ef4444'], ['Neutral', counts.Neutral, '#4a5568']] as const).map(([side, count, color]) => {
                const pct = total > 0 ? count / total * 100 : 0
                return (
                  <div key={side}>
                    <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.75rem', marginBottom: 4 }}>
                      <span style={{ color }}>{side}</span>
                      <span className="font-mono-vs" style={{ color: 'var(--text-muted)' }}>{count} <span style={{ color: 'var(--text-dim)' }}>({Math.round(pct)}%)</span></span>
                    </div>
                    <div style={{ height: 6, background: 'var(--bg-elevated)', borderRadius: 2, overflow: 'hidden' }}>
                      <div style={{ width: `${pct}%`, height: '100%', background: color, transition: 'width 0.5s' }} />
                    </div>
                  </div>
                )
              })}
            </div>
          </Panel>

          {/* Health distribution */}
          <Panel title="Health Distribution" icon={Objective} iconColor="#4ade80">
            <div className="p-5">
              <ResponsiveContainer width="100%" height={100}>
                <BarChart data={healthBuckets} margin={{ left: -10, right: 4 }}>
                  <XAxis dataKey="range" tick={{ fill: 'var(--text-dim)', fontSize: 10 }} axisLine={false} tickLine={false} />
                  <YAxis tick={{ fill: 'var(--text-dim)', fontSize: 11 }} axisLine={false} tickLine={false} width={20} />
                  <Tooltip {...TT} />
                  <Bar dataKey="count" radius={[3, 3, 0, 0]}>
                    {healthBuckets.map((b, i) => <Cell key={i} fill={b.fill} />)}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </div>
          </Panel>

          {/* Objective type breakdown */}
          <Panel title="Types" icon={Pin} iconColor="#22d3ee">
            <div className="p-5">
              {kindData.length > 0 ? (
                <ResponsiveContainer width="100%" height={100}>
                  <PieChart>
                    <Pie data={kindData} cx="50%" cy="50%" outerRadius={44} paddingAngle={2} dataKey="count" stroke="none">
                      {kindData.map((d, i) => <Cell key={i} fill={d.fill} />)}
                    </Pie>
                    <Tooltip {...TT} formatter={(v, name) => [v, name]} />
                  </PieChart>
                </ResponsiveContainer>
              ) : (
                <div style={{ height: 100, display: 'flex', alignItems: 'center', justifyContent: 'center', color: 'var(--text-dim)', fontSize: '0.8rem' }}>No data</div>
              )}
              <div style={{ display: 'flex', flexWrap: 'wrap', gap: '4px 12px', marginTop: 8 }}>
                {kindData.map(d => (
                  <div key={d.name} style={{ display: 'flex', alignItems: 'center', gap: 4, fontSize: '0.6rem' }}>
                    <span style={{ width: 8, height: 8, borderRadius: '50%', background: d.fill, display: 'inline-block' }} />
                    <span style={{ color: 'var(--text-dim)' }}>{d.name} <span className="font-mono-vs" style={{ color: 'var(--text-muted)' }}>{d.count}</span></span>
                  </div>
                ))}
              </div>
            </div>
          </Panel>
        </div>

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
                onClick={() => setFilter(f)}
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '0.75rem',
                  letterSpacing: '0.1em',
                  padding: '0.3rem 0.75rem',
                  background: filter === f ? 'rgba(56,189,248,0.1)' : 'transparent',
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
          <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.1em', marginLeft: '0.25rem' }}>
            click a column header to sort
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
                  colSpan={9} what="objectives" isLoading={isLoading}
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
                      <td className="font-mono-vs" style={{ padding: '9px 14px', fontSize: '0.62rem', color: 'var(--text-dim)', whiteSpace: 'nowrap' }}>
                        {new Date(obj.last_change).toLocaleString()}
                      </td>
                    </tr>
                  )
                })}
                <QueryState
                  colSpan={9} what="objectives" isLoading={false}
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
