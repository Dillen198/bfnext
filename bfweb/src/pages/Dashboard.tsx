import React, { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  AreaChart, Area, BarChart, Bar, PieChart, Pie, Cell,
  XAxis, YAxis, Tooltip, ResponsiveContainer, Legend,
} from 'recharts'
import {
  Shield, Target, Users, Zap, Clock,
  AlertTriangle, Award, Crosshair, Activity, MapPin, Flame,
} from 'lucide-react'
import { api } from '../api'
import SideBadge from '../components/SideBadge'
import HealthBar from '../components/HealthBar'
import PageHeader from '../components/PageHeader'

const TT = {
  contentStyle: { background: '#0a1628', border: '1px solid #1a3555', borderRadius: 6, color: '#c9d1d9', fontSize: 12 },
  labelStyle: { color: '#4b5563' },
  cursor: { fill: 'rgba(59,130,246,0.05)' },
}

// ── Shared card shell ──
function Card({
  children, className = '', accent = '#3b82f6',
}: { children: React.ReactNode; className?: string; accent?: string }) {
  return (
    <div className={`tac-card ${className}`} style={{ '--accent': accent } as React.CSSProperties}>
      {children}
    </div>
  )
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

// ── KPI Card ──
function KpiCard({ label, value, icon: Icon, color, sub }: {
  label: string; value: React.ReactNode; icon: React.ElementType; color: string; sub?: string
}) {
  return (
    <div className="tac-card p-4" style={{ borderColor: `${color}22` } as React.CSSProperties}>
      <div className="absolute inset-x-0 top-0 h-px" style={{ background: `linear-gradient(to right,transparent,${color}55,transparent)` }} />
      <div className="flex items-center justify-between mb-3">
        <span className="text-[9px] text-slate-600 uppercase tracking-widest">{label}</span>
        <div className="p-1.5 rounded" style={{ background: `${color}15` }}>
          <Icon size={11} style={{ color }} />
        </div>
      </div>
      <div className="text-[28px] leading-none font-bold font-mono tabular-nums mb-1" style={{ color }}>{value}</div>
      {sub && <div className="text-[10px] text-slate-600 mt-1">{sub}</div>}
    </div>
  )
}

// ── Objective kind icon ──
const kindBg: Record<string, string> = {
  Airbase: '#1e3a5f', FARP: '#1a3020', FOB: '#2d2010',
  Factory: '#2a1a30', 'Logistics Hub': '#1a2a2d', 'Naval Base': '#101d30',
  'Carrier Group': '#101d30',
}
const kindLabel: Record<string, string> = {
  Airbase: 'AB', FARP: 'FP', FOB: 'FB', Factory: 'FC',
  'Logistics Hub': 'LH', 'Naval Base': 'NB', 'Carrier Group': 'CG',
}

export default function Dashboard() {
  const { data: stats } = useQuery({ queryKey: ['stats'], queryFn: api.stats, refetchInterval: 30_000 })
  const { data: pilots = [] } = useQuery({ queryKey: ['leaderboard'], queryFn: api.leaderboard, refetchInterval: 30_000 })
  const { data: objectives = [] } = useQuery({ queryKey: ['objectives'], queryFn: () => api.objectives(), refetchInterval: 30_000 })
  const { data: rounds = [] } = useQuery({ queryKey: ['rounds'], queryFn: api.rounds, refetchInterval: 60_000 })
  const { data: kills = [] } = useQuery({ queryKey: ['kills'], queryFn: () => api.kills(undefined, 50), refetchInterval: 15_000 })

  const activeRound = rounds.find(r => r.active)
  const pastRounds = rounds.filter(r => !r.active).slice(-3).reverse()

  // ── Territory ──
  const redCount     = objectives.filter(o => o.owner === 'Red').length
  const blueCount    = objectives.filter(o => o.owner === 'Blue').length
  const neutralCount = objectives.filter(o => o.owner === 'Neutral').length
  const total        = redCount + blueCount + neutralCount

  const bluePct    = total > 0 ? Math.round(blueCount    / total * 100) : 0
  const redPct     = total > 0 ? Math.round(redCount     / total * 100) : 0
  const neutralPct = total > 0 ? Math.round(neutralCount / total * 100) : 0

  // ── Pilot name map ──
  const nameMap = useMemo(() => {
    const m = new Map<string, string>()
    pilots.forEach(p => m.set(p.ucid, p.name))
    return m
  }, [pilots])

  // ── Kill activity by hour ──
  const hourBuckets: Record<number, { blue: number; red: number }> = {}
  kills.forEach(k => {
    const h = new Date(k.time).getHours()
    if (!hourBuckets[h]) hourBuckets[h] = { blue: 0, red: 0 }
    if (k.killer?.side === 'Blue') hourBuckets[h].blue++
    else if (k.killer?.side === 'Red') hourBuckets[h].red++
  })
  const activityData = Array.from({ length: 24 }, (_, i) => ({
    hour: `${i}h`,
    blue: hourBuckets[i]?.blue ?? 0,
    red:  hourBuckets[i]?.red  ?? 0,
  }))

  // ── Top pilots bar chart ──
  const top8 = pilots.slice(0, 8).map(p => ({
    name: p.name.length > 14 ? p.name.slice(0, 13) + '…' : p.name,
    air: p.air_kills,
    ground: p.ground_kills,
  }))

  // ── Weapon distribution ──
  const weaponCounts: Record<string, number> = {}
  kills.forEach(k => { const w = k.killer?.weapon ?? 'Unknown'; weaponCounts[w] = (weaponCounts[w] ?? 0) + 1 })
  const topWeapons = Object.entries(weaponCounts).sort((a, b) => b[1] - a[1]).slice(0, 6)
    .map(([name, count]) => ({ name: name.length > 18 ? name.slice(0, 17) + '…' : name, count }))

  // ── Objectives under threat ──
  const threatened = [...objectives].filter(o => o.health < 60).sort((a, b) => a.health - b.health).slice(0, 8)

  // ── Summary stats ──
  const totalHours    = pilots.reduce((s, p) => s + p.hours, 0)
  const totalAirKills = pilots.reduce((s, p) => s + p.air_kills, 0)
  const totalGndKills = pilots.reduce((s, p) => s + p.ground_kills, 0)
  const blueKills     = kills.filter(k => k.killer?.side === 'Blue').length
  const redKills      = kills.filter(k => k.killer?.side === 'Red').length

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="SITREP"
        sub={activeRound
          ? `${activeRound.scenario} · active since ${new Date(activeRound.start).toLocaleDateString()}`
          : 'No active campaign'}
        right={
          activeRound ? (
            <span className="status-live">
              <span className="w-1.5 h-1.5 rounded-full bg-green-400 glow-pulse" />
              CAMPAIGN ACTIVE
            </span>
          ) : (
            <span className="status-offline">STANDBY</span>
          )
        }
      />

      <div className="flex-1 overflow-auto p-4 space-y-3 grid-bg">

        {/* ── KPI strip ── */}
        <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-3">
          <KpiCard label="Active Pilots"   value={stats?.total_pilots ?? '—'}   icon={Users}    color="#3b82f6" sub={`${totalHours.toFixed(0)}h total`} />
          <KpiCard label="Total Kills"     value={stats?.total_kills ?? '—'}    icon={Zap}      color="#ef4444" sub={`${totalAirKills} air · ${totalGndKills} gnd`} />
          <KpiCard label="Blue Territory"  value={`${bluePct}%`}                icon={Shield}   color="#60a5fa" sub={`${blueCount} / ${total} obj`} />
          <KpiCard label="Red Territory"   value={`${redPct}%`}                 icon={Flame}    color="#f87171" sub={`${redCount} / ${total} obj`} />
          <KpiCard label="Objectives"      value={stats?.objective_count ?? '—'} icon={Target}  color="#fbbf24" sub={`${neutralCount} neutral`} />
          <KpiCard label="Rounds Played"   value={stats?.total_rounds ?? '—'}   icon={Clock}    color="#a78bfa" sub={activeRound ? 'Round in progress' : 'No active round'} />
        </div>

        {/* ── Territory control bar ── */}
        {total > 0 && (
          <div className="tac-card px-4 py-3">
            <div className="flex items-center justify-between mb-2">
              <span className="text-[10px] text-slate-600 uppercase tracking-widest flex items-center gap-1.5">
                <MapPin size={10} className="text-slate-600" />
                Territory Control
              </span>
              <div className="flex items-center gap-4 text-[10px] font-mono">
                <span className="text-blue-400">{blueCount} Blue</span>
                <span className="text-slate-600">·</span>
                <span className="text-red-400">{redCount} Red</span>
                <span className="text-slate-600">·</span>
                <span className="text-slate-500">{neutralCount} Neutral</span>
              </div>
            </div>
            <div className="h-3 rounded-full overflow-hidden flex bg-[#4b5563]/15 gap-px">
              <div className="h-full rounded-l-full transition-all duration-700" style={{ width: `${bluePct}%`, background: 'linear-gradient(90deg, #1d4ed8, #3b82f6)' }} />
              <div className="h-full transition-all duration-700" style={{ width: `${neutralPct}%`, background: '#374151' }} />
              <div className="h-full rounded-r-full transition-all duration-700" style={{ width: `${redPct}%`, background: 'linear-gradient(90deg, #ef4444, #b91c1c)' }} />
            </div>
          </div>
        )}

        {/* ── Main charts row ── */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-3">
          {/* Top pilots */}
          <div className="lg:col-span-2">
            <Card>
              <CardHeader title="Top Pilots by Kills" icon={Award} color="text-yellow-400" />
              <div className="p-4">
                {top8.length > 0 ? (
                  <ResponsiveContainer width="100%" height={220}>
                    <BarChart data={top8} layout="vertical" margin={{ left: 0, right: 16, top: 4, bottom: 4 }}>
                      <XAxis type="number" tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} />
                      <YAxis type="category" dataKey="name" tick={{ fill: '#94a3b8', fontSize: 11 }} width={100} axisLine={false} tickLine={false} />
                      <Tooltip {...TT} />
                      <Bar dataKey="air"    name="Air Kills"    stackId="a" fill="#3b82f6" />
                      <Bar dataKey="ground" name="Ground Kills" stackId="a" fill="#f97316" radius={[0, 4, 4, 0]} />
                      <Legend wrapperStyle={{ fontSize: 10, color: '#64748b' }} iconSize={8} />
                    </BarChart>
                  </ResponsiveContainer>
                ) : (
                  <div className="h-[220px] flex items-center justify-center text-slate-700 text-xs">No pilot data</div>
                )}
              </div>
            </Card>
          </div>

          {/* Kill distribution + recent round */}
          <div className="space-y-3">
            <Card>
              <CardHeader title="Blue vs Red Kills" icon={Crosshair} color="text-red-400" />
              <div className="p-4">
                {(blueKills + redKills) > 0 ? (
                  <>
                    <ResponsiveContainer width="100%" height={100}>
                      <PieChart>
                        <Pie data={[
                          { name: 'Blue', value: blueKills, color: '#3b82f6' },
                          { name: 'Red',  value: redKills,  color: '#ef4444' },
                        ]} cx="50%" cy="50%" innerRadius={28} outerRadius={44} paddingAngle={3} dataKey="value" stroke="none">
                          <Cell fill="#3b82f6" />
                          <Cell fill="#ef4444" />
                        </Pie>
                        <Tooltip {...TT} />
                      </PieChart>
                    </ResponsiveContainer>
                    <div className="flex justify-center gap-6 text-[11px] mt-1">
                      <div className="text-center">
                        <div className="font-mono font-bold text-blue-400 text-base">{blueKills}</div>
                        <div className="text-slate-600">Blue</div>
                      </div>
                      <div className="text-center">
                        <div className="font-mono font-bold text-red-400 text-base">{redKills}</div>
                        <div className="text-slate-600">Red</div>
                      </div>
                    </div>
                  </>
                ) : (
                  <div className="h-[130px] flex items-center justify-center text-slate-700 text-xs">No kill data</div>
                )}
              </div>
            </Card>

            {/* Past rounds */}
            {pastRounds.length > 0 && (
              <Card>
                <CardHeader title="Past Rounds" icon={Clock} color="text-purple-400" />
                <div className="divide-y divide-[#0d1a2d]">
                  {pastRounds.map(r => (
                    <div key={r.id} className="px-4 py-2 flex items-center justify-between">
                      <div>
                        <div className="text-[11px] text-slate-300 font-semibold truncate max-w-[120px]">{r.scenario}</div>
                        <div className="text-[9px] text-slate-600 mt-0.5">{new Date(r.start).toLocaleDateString()}</div>
                      </div>
                      {r.winner ? (
                        <span className={`text-[10px] font-bold px-2 py-0.5 rounded ${
                          r.winner.includes('Blue') ? 'text-blue-300 bg-blue-500/10' :
                          r.winner.includes('Red')  ? 'text-red-300 bg-red-500/10'  :
                          'text-slate-400 bg-slate-500/10'
                        }`}>{r.winner}</span>
                      ) : (
                        <span className="text-[10px] text-slate-700">—</span>
                      )}
                    </div>
                  ))}
                </div>
              </Card>
            )}
          </div>
        </div>

        {/* ── Kill activity + weapons ── */}
        <div className="grid grid-cols-1 lg:grid-cols-5 gap-3">
          {/* Kill activity area chart */}
          <Card className="lg:col-span-3">
            <CardHeader title="Kill Activity (24h)" icon={Activity} color="text-cyan-400" />
            <div className="p-4">
              <ResponsiveContainer width="100%" height={140}>
                <AreaChart data={activityData} margin={{ left: -10, right: 4, top: 4, bottom: 0 }}>
                  <defs>
                    <linearGradient id="blueGrad" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%"  stopColor="#3b82f6" stopOpacity={0.3} />
                      <stop offset="95%" stopColor="#3b82f6" stopOpacity={0} />
                    </linearGradient>
                    <linearGradient id="redGrad" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%"  stopColor="#ef4444" stopOpacity={0.3} />
                      <stop offset="95%" stopColor="#ef4444" stopOpacity={0} />
                    </linearGradient>
                  </defs>
                  <XAxis dataKey="hour" tick={{ fill: '#374151', fontSize: 9 }} axisLine={false} tickLine={false} interval={3} />
                  <YAxis tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} />
                  <Tooltip {...TT} />
                  <Area type="monotone" dataKey="blue" name="Blue" stroke="#3b82f6" strokeWidth={1.5} fill="url(#blueGrad)" dot={false} />
                  <Area type="monotone" dataKey="red"  name="Red"  stroke="#ef4444" strokeWidth={1.5} fill="url(#redGrad)"  dot={false} />
                </AreaChart>
              </ResponsiveContainer>
            </div>
          </Card>

          {/* Top weapons */}
          <Card className="lg:col-span-2">
            <CardHeader title="Top Weapons" icon={Zap} color="text-orange-400" />
            <div className="p-3">
              {topWeapons.length > 0 ? (
                <div className="space-y-1.5">
                  {topWeapons.map((w, i) => {
                    const pct = (topWeapons[0]?.count ?? 0) > 0 ? (w.count / topWeapons[0]!.count) * 100 : 0
                    return (
                      <div key={i}>
                        <div className="flex justify-between text-[10px] mb-0.5">
                          <span className="text-slate-400 truncate max-w-[150px]">{w.name}</span>
                          <span className="font-mono text-slate-300 ml-2">{w.count}</span>
                        </div>
                        <div className="h-1 bg-[#0d1117] rounded-full overflow-hidden">
                          <div
                            className="h-full rounded-full transition-all duration-500"
                            style={{ width: `${pct}%`, background: 'linear-gradient(90deg, #f97316, #ef4444)' }}
                          />
                        </div>
                      </div>
                    )
                  })}
                </div>
              ) : (
                <div className="h-[120px] flex items-center justify-center text-slate-700 text-xs">No data</div>
              )}
            </div>
          </Card>
        </div>

        {/* ── Bottom row: recent kills + threatened objectives ── */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-3">

          {/* Recent kill feed */}
          <Card>
            <CardHeader title="Recent Kills" icon={Crosshair} color="text-red-400"
              right={<span className="text-[9px] text-slate-700 font-mono">LIVE · 15s</span>}
            />
            <div className="divide-y divide-[#0a1520]">
              {kills.length === 0 && (
                <div className="px-4 py-6 text-center text-slate-700 text-xs">No kills recorded</div>
              )}
              {kills.slice(0, 12).map((k, i) => {
                const killerName = k.killer?.ucid ? (nameMap.get(k.killer.ucid) ?? 'Unknown Pilot') : null
                const victimName = k.victim.ucid ? (nameMap.get(k.victim.ucid) ?? 'Unknown') : null
                return (
                  <div key={i} className="kill-row flex items-center gap-2 px-3 py-2">
                    <span className="text-[9px] font-mono text-slate-700 w-14 shrink-0 tabular-nums">
                      {new Date(k.time).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' })}
                    </span>
                    <SideBadge side={k.killer?.side ?? 'Neutral'} size="xs" />
                    <span className="text-[10px] flex-1 min-w-0 truncate">
                      {killerName && <span className="text-slate-200 font-semibold">{killerName} </span>}
                      {!killerName && <span className="text-slate-600">AI </span>}
                      {k.killer?.weapon && (
                        <span className="text-yellow-500/70 font-mono text-[9px]">[{k.killer.weapon}]</span>
                      )}
                      <span className="text-slate-700"> → </span>
                      <span className="text-slate-400">
                        {victimName ?? k.target_type ?? 'Unknown'}
                      </span>
                    </span>
                    <SideBadge side={k.victim.side} size="xs" />
                  </div>
                )
              })}
            </div>
          </Card>

          {/* Threatened objectives */}
          <Card>
            <CardHeader title="Objectives Under Threat" icon={AlertTriangle} color="text-amber-400"
              right={<span className="text-[9px] text-amber-600 font-mono">{threatened.length} CRITICAL</span>}
            />
            <div className="divide-y divide-[#0a1520] max-h-[340px] overflow-y-auto">
              {threatened.length === 0 ? (
                <div className="px-4 py-6 text-center text-slate-700 text-xs">All objectives healthy</div>
              ) : (
                threatened.map(obj => (
                  <div key={obj.id} className="flex items-center gap-3 px-3 py-2 hover:bg-amber-500/[0.02]">
                    <div
                      className="obj-icon text-[9px] font-bold text-slate-300"
                      style={{ background: kindBg[obj.kind] ?? '#1a2030' }}
                    >
                      {kindLabel[obj.kind] ?? '??'}
                    </div>
                    <div className="flex-1 min-w-0">
                      <div className="flex items-center gap-2 mb-1">
                        <span className="text-[11px] font-semibold text-slate-200 truncate">{obj.name}</span>
                        <SideBadge side={obj.owner} size="xs" />
                      </div>
                      <div className="flex items-center gap-2">
                        <div className="flex-1">
                          <HealthBar value={obj.health} />
                        </div>
                        <span className="text-[9px] font-mono text-slate-600">
                          L:{obj.logi}% S:{obj.supply}%
                        </span>
                      </div>
                    </div>
                  </div>
                ))
              )}
            </div>
          </Card>
        </div>

      </div>
    </div>
  )
}
