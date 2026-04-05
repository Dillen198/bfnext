import { useQuery } from '@tanstack/react-query'
import { useState, useMemo } from 'react'
import { api } from '../api'
import HealthBar from '../components/HealthBar'
import SideBadge from '../components/SideBadge'
import PageHeader from '../components/PageHeader'
import { BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell, PieChart, Pie } from 'recharts'
import { Target, AlertTriangle, Shield, MapPin } from 'lucide-react'

const TT = {
  contentStyle: { background: '#0a1628', border: '1px solid #1a3555', borderRadius: 6, color: '#c9d1d9', fontSize: 12 },
  cursor: { fill: 'rgba(59,130,246,0.05)' },
}

type Filter = 'All' | 'Red' | 'Blue' | 'Neutral'

const OBJ_KINDS = ['Airbase', 'FARP', 'FOB', 'Factory', 'Logistics Hub', 'Naval Base', 'Carrier Group']
const KIND_ICONS: Record<string, string> = {
  Airbase: '✈', FARP: '⬡', FOB: '▲', Factory: '⚙',
  'Logistics Hub': '◈', 'Naval Base': '⚓', 'Carrier Group': '⚓',
}
const KIND_COLORS: Record<string, string> = {
  Airbase: '#3b82f6', FARP: '#22c55e', FOB: '#f97316', Factory: '#a78bfa',
  'Logistics Hub': '#06b6d4', 'Naval Base': '#0ea5e9', 'Carrier Group': '#0ea5e9',
}

function Card({ children, className = '' }: { children: React.ReactNode; className?: string }) {
  return <div className={`tac-card ${className}`}>{children}</div>
}
function CardHeader({ title, icon: Icon, color = 'text-blue-400', right }: {
  title: string; icon: React.ElementType; color?: string; right?: React.ReactNode
}) {
  return (
    <div className="flex items-center justify-between px-4 pt-3.5 pb-3 border-b border-[#1e3a5f]/40">
      <div className="flex items-center gap-2">
        <Icon size={12} className={color} />
        <span className="text-[10px] font-semibold tracking-[0.15em] text-slate-500 uppercase">{title}</span>
      </div>
      {right}
    </div>
  )
}

export default function Objectives() {
  const { data: objectives = [], isLoading } = useQuery({
    queryKey: ['objectives'],
    queryFn: () => api.objectives(),
    refetchInterval: 30_000,
  })
  const [filter, setFilter] = useState<Filter>('All')
  const [search, setSearch] = useState('')
  const [sortBy, setSortBy] = useState<'name' | 'health' | 'logi' | 'supply' | 'fuel'>('health')

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
      if (sortBy === 'health')  return b.health - a.health
      if (sortBy === 'logi')    return b.logi - a.logi
      if (sortBy === 'supply')  return b.supply - a.supply
      if (sortBy === 'fuel')    return b.fuel - a.fuel
      return a.name.localeCompare(b.name)
    }),
    [objectives, filter, search, sortBy]
  )

  const bluePct    = total > 0 ? counts.Blue    / total * 100 : 0
  const redPct     = total > 0 ? counts.Red     / total * 100 : 0
  const neutralPct = total > 0 ? counts.Neutral / total * 100 : 0

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="OBJECTIVES"
        sub={`${total} total · ${counts.Red} red · ${counts.Blue} blue · ${counts.Neutral} neutral`}
        right={
          <input
            type="text"
            placeholder="Search…"
            value={search}
            onChange={e => setSearch(e.target.value)}
            className="bg-[#050d1a] border border-[#1e3a5f]/60 rounded px-3 py-1.5 text-[11px] text-slate-300 placeholder:text-slate-700 focus:outline-none focus:border-blue-500/50 w-36"
          />
        }
      />

      <div className="flex-1 overflow-auto p-4 space-y-3 grid-bg">

        {/* ── Territory bar ── */}
        {total > 0 && (
          <div className="tac-card px-4 py-3">
            <div className="flex items-center justify-between mb-2">
              <span className="text-[10px] text-slate-600 uppercase tracking-widest flex items-center gap-1.5">
                <MapPin size={10} className="text-slate-600" />
                Territory Control
              </span>
              <div className="flex items-center gap-4 text-[10px] font-mono">
                <span className="text-blue-400">{Math.round(bluePct)}% Blue</span>
                <span className="text-slate-600">·</span>
                <span className="text-red-400">{Math.round(redPct)}% Red</span>
                <span className="text-slate-600">·</span>
                <span className="text-slate-500">{Math.round(neutralPct)}% Neutral</span>
              </div>
            </div>
            <div className="h-3 rounded-full overflow-hidden flex bg-[#4b5563]/15">
              <div className="h-full transition-all duration-700" style={{ width: `${bluePct}%`, background: 'linear-gradient(90deg,#1d4ed8,#3b82f6)' }} />
              <div className="h-full transition-all duration-700" style={{ width: `${neutralPct}%`, background: '#374151' }} />
              <div className="h-full transition-all duration-700" style={{ width: `${redPct}%`, background: 'linear-gradient(90deg,#ef4444,#b91c1c)' }} />
            </div>
          </div>
        )}

        {/* ── Critical alerts ── */}
        {criticalObjs.length > 0 && (
          <Card>
            <CardHeader
              title="Critical Objectives"
              icon={AlertTriangle}
              color="text-amber-400"
              right={<span className="text-[9px] text-amber-600 font-mono">{criticalObjs.length} CRITICAL</span>}
            />
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-0 divide-y sm:divide-y-0 divide-[#0a1520]">
              {criticalObjs.map(obj => (
                <div key={obj.id} className="flex items-center gap-3 px-4 py-3 border-b border-[#0a1520] hover:bg-amber-500/[0.02]">
                  <div className="text-lg" style={{ color: KIND_COLORS[obj.kind] ?? '#4b5563' }}>
                    {KIND_ICONS[obj.kind] ?? '■'}
                  </div>
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 mb-1.5">
                      <span className="text-[12px] font-semibold text-slate-100 truncate">{obj.name}</span>
                      <SideBadge side={obj.owner} size="xs" />
                    </div>
                    <HealthBar value={obj.health} />
                    <div className="flex gap-3 mt-1 text-[9px] font-mono text-slate-600">
                      <span>L:{obj.logi}%</span>
                      <span>S:{obj.supply}%</span>
                      <span>F:{obj.fuel}%</span>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </Card>
        )}

        {/* ── Charts row ── */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-3">
          {/* Ownership bars */}
          <Card>
            <CardHeader title="Ownership" icon={Shield} color="text-blue-400" />
            <div className="px-4 py-3 space-y-2.5">
              {([['Blue', counts.Blue, '#3b82f6'], ['Red', counts.Red, '#ef4444'], ['Neutral', counts.Neutral, '#4b5563']] as const).map(([side, count, color]) => {
                const pct = total > 0 ? count / total * 100 : 0
                return (
                  <div key={side}>
                    <div className="flex justify-between text-[11px] mb-1">
                      <span style={{ color }}>{side}</span>
                      <span className="font-mono text-slate-400">{count} <span className="text-slate-600">({Math.round(pct)}%)</span></span>
                    </div>
                    <div className="h-2 bg-[#0a1117] rounded-full overflow-hidden">
                      <div className="h-full rounded-full transition-all duration-500" style={{ width: `${pct}%`, background: color }} />
                    </div>
                  </div>
                )
              })}
            </div>
          </Card>

          {/* Health distribution */}
          <Card>
            <CardHeader title="Health Distribution" icon={Target} color="text-green-400" />
            <div className="p-4">
              <ResponsiveContainer width="100%" height={100}>
                <BarChart data={healthBuckets} margin={{ left: -10, right: 4 }}>
                  <XAxis dataKey="range" tick={{ fill: '#374151', fontSize: 9 }} axisLine={false} tickLine={false} />
                  <YAxis tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} width={20} />
                  <Tooltip {...TT} />
                  <Bar dataKey="count" radius={[3, 3, 0, 0]}>
                    {healthBuckets.map((b, i) => <Cell key={i} fill={b.fill} />)}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </div>
          </Card>

          {/* Objective type breakdown */}
          <Card>
            <CardHeader title="Types" icon={MapPin} color="text-cyan-400" />
            <div className="p-4">
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
                <div className="h-[100px] flex items-center justify-center text-slate-700 text-xs">No data</div>
              )}
              <div className="flex flex-wrap gap-x-3 gap-y-0.5 mt-1">
                {kindData.map(d => (
                  <div key={d.name} className="flex items-center gap-1 text-[9px]">
                    <span className="w-1.5 h-1.5 rounded-full" style={{ background: d.fill }} />
                    <span className="text-slate-600">{d.name} <span className="text-slate-400 font-mono">{d.count}</span></span>
                  </div>
                ))}
              </div>
            </div>
          </Card>
        </div>

        {/* ── Controls ── */}
        <div className="flex items-center gap-2 flex-wrap">
          <div className="flex rounded border border-[#1e3a5f]/50 overflow-hidden text-[9px] font-bold tracking-widest">
            {(['All', 'Red', 'Blue', 'Neutral'] as Filter[]).map(f => (
              <button
                key={f}
                onClick={() => setFilter(f)}
                className={`px-3 py-1.5 transition-colors ${filter === f ? 'bg-blue-500/15 text-blue-300' : 'text-slate-600 hover:text-slate-400 hover:bg-white/[0.02]'}`}
              >
                {f.toUpperCase()}
              </button>
            ))}
          </div>
          <span className="text-[9px] text-slate-700 uppercase tracking-widest ml-1">Sort:</span>
          <div className="flex rounded border border-[#1e3a5f]/50 overflow-hidden text-[9px] font-bold tracking-widest">
            {(['health', 'logi', 'supply', 'fuel', 'name'] as const).map(s => (
              <button
                key={s}
                onClick={() => setSortBy(s)}
                className={`px-3 py-1.5 transition-colors ${sortBy === s ? 'bg-blue-500/15 text-blue-300' : 'text-slate-600 hover:text-slate-400 hover:bg-white/[0.02]'}`}
              >
                {s.toUpperCase()}
              </button>
            ))}
          </div>
          <span className="ml-auto text-[10px] text-slate-700 font-mono">{filtered.length} shown</span>
        </div>

        {/* ── Table ── */}
        <div className="tac-card overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead className="border-b border-[#1e3a5f]/40">
                <tr>
                  {['Name', 'Type', 'Owner', 'Health', 'Logistics', 'Supply', 'Fuel', 'Last Change'].map(h => (
                    <th key={h} className="px-3 py-2.5 text-left text-[9px] uppercase tracking-widest text-slate-700 whitespace-nowrap">{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {isLoading && (
                  <tr><td colSpan={8} className="text-center py-10 text-slate-700 text-xs">Loading…</td></tr>
                )}
                {filtered.map(obj => {
                  const isCrit = obj.health < 40
                  return (
                    <tr key={obj.id} className={`border-b border-[#080f1b] kill-row transition-colors ${isCrit ? 'bg-red-500/[0.02]' : ''}`}>
                      <td className="px-3 py-2">
                        <div className="flex items-center gap-2">
                          <span style={{ color: KIND_COLORS[obj.kind] ?? '#4b5563' }}>{KIND_ICONS[obj.kind] ?? '■'}</span>
                          <span className="text-[12px] font-semibold text-slate-100">{obj.name}</span>
                          {isCrit && <AlertTriangle size={10} className="text-amber-500 flex-shrink-0" />}
                        </div>
                      </td>
                      <td className="px-3 py-2 text-[10px] text-slate-600">{obj.kind}</td>
                      <td className="px-3 py-2"><SideBadge side={obj.owner} size="xs" /></td>
                      <td className="px-3 py-2 w-32"><HealthBar value={obj.health} /></td>
                      <td className="px-3 py-2 w-32"><HealthBar value={obj.logi} /></td>
                      <td className="px-3 py-2 w-32"><HealthBar value={obj.supply} /></td>
                      <td className="px-3 py-2 w-32"><HealthBar value={obj.fuel} /></td>
                      <td className="px-3 py-2 text-[9px] text-slate-700 whitespace-nowrap font-mono">
                        {new Date(obj.last_change).toLocaleString()}
                      </td>
                    </tr>
                  )
                })}
                {!isLoading && filtered.length === 0 && (
                  <tr><td colSpan={8} className="text-center py-10 text-slate-700 text-xs">No objectives match</td></tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  )
}
