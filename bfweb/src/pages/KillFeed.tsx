import React, { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import SideBadge from '../components/SideBadge'
import PageHeader from '../components/PageHeader'
import {
  BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer,
  AreaChart, Area,
} from 'recharts'
import { Crosshair, Zap, Activity, Users } from 'lucide-react'
import { useRound } from '../context/RoundContext'

const TT = {
  contentStyle: { background: '#111111', border: '1px solid #2a2a2a', borderRadius: 2, color: '#f1f5f9', fontSize: 12 },
  cursor: { fill: 'rgba(77,124,15,0.04)' },
}

function Card({ children, className = '' }: { children: React.ReactNode; className?: string }) {
  return <div className={`vs-card ${className}`}>{children}</div>
}
function CardHeader({ title, icon: Icon, color = 'text-slate-400', right }: {
  title: string; icon: React.ElementType; color?: string; right?: React.ReactNode
}) {
  return (
    <div className="flex items-center justify-between px-5 pt-4 pb-3.5" style={{ borderBottom: '1px solid var(--border)' }}>
      <div className="flex items-center gap-2">
        <Icon size={13} className={color} />
        <span className="vs-section-title" style={{ fontSize: '0.72rem' }}>{title}</span>
      </div>
      {right}
    </div>
  )
}

// Classify kill type from target_type string
function classifyKill(targetType: string | null, weapon: string | null): { label: string; color: string } {
  const t = (targetType ?? '').toLowerCase()
  const w = (weapon ?? '').toLowerCase()
  if (t.includes('air') || t.includes('plane') || t.includes('heli') || t.includes('fighter') || w.includes('aa') || w.includes('aim') || w.includes('r-'))
    return { label: 'AIR', color: '#3b82f6' }
  if (t.includes('ship') || t.includes('naval') || t.includes('carrier'))
    return { label: 'NAVAL', color: '#06b6d4' }
  if (t.includes('armor') || t.includes('tank') || t.includes('apc') || t.includes('ifv'))
    return { label: 'ARMOR', color: '#f97316' }
  if (t.includes('sam') || t.includes('radar') || t.includes('aaa') || t.includes('air defence'))
    return { label: 'AD', color: '#a78bfa' }
  if (t.includes('truck') || t.includes('supply') || t.includes('vehicle') || t.includes('car'))
    return { label: 'VEHICLE', color: '#fbbf24' }
  if (t.includes('infantry') || t.includes('troop') || t.includes('soldier'))
    return { label: 'INF', color: '#22c55e' }
  return { label: 'GND', color: '#94a3b8' }
}

export default function KillFeed() {
  const { selectedRound } = useRound()
  const { data: kills = [], isLoading, dataUpdatedAt } = useQuery({
    queryKey: ['kills-feed', selectedRound],
    queryFn: () => api.kills(selectedRound, 150),
    refetchInterval: 15_000,
  })
  const { data: allPilots = [] } = useQuery({
    queryKey: ['all-pilots'],
    queryFn: api.allPilots,
    refetchInterval: 120_000,
  })

  // Pilot name resolution
  const nameMap = useMemo(() => {
    const m = new Map<string, string>()
    allPilots.forEach(p => m.set(p.ucid, p.name))
    return m
  }, [allPilots])

  // Weapon kill counts
  const weaponCounts: Record<string, number> = {}
  kills.forEach(k => {
    const w = k.killer?.weapon ?? 'Unknown'
    weaponCounts[w] = (weaponCounts[w] ?? 0) + 1
  })
  const topWeapons = Object.entries(weaponCounts)
    .sort((a, b) => b[1] - a[1]).slice(0, 10)
    .map(([name, count]) => ({ name: name.length > 22 ? name.slice(0, 21) + '…' : name, count }))

  // Kill activity by hour
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

  // Side totals
  const blueKills = kills.filter(k => k.killer?.side === 'Blue').length
  const redKills  = kills.filter(k => k.killer?.side === 'Red').length
  const playerKills = kills.filter(k => k.killer?.ucid).length
  const aiKills     = kills.filter(k => k.killer && !k.killer.ucid).length

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="KILL FEED"
        sub={`${kills.length} events · auto-refreshes every 15s`}
        right={
          <span className="text-[11px] text-slate-600 font-mono">
            {dataUpdatedAt ? `UPD ${new Date(dataUpdatedAt).toLocaleTimeString()}` : ''}
          </span>
        }
      />

      <div className="flex-1 overflow-auto p-4 space-y-4 grid-bg" style={{ background: 'var(--bg)' }}>
        {/* ── Stats row ── */}
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-4">
          {[
            { label: 'Blue Kills', value: blueKills, color: '#3b82f6', icon: Crosshair },
            { label: 'Red Kills',  value: redKills,  color: '#ef4444', icon: Crosshair },
            { label: 'Player Kills', value: playerKills, color: '#22c55e', icon: Users },
            { label: 'AI Kills',   value: aiKills,   color: '#a78bfa', icon: Zap },
          ].map(s => (
            <div key={s.label} className="vs-card-top p-4" style={{ borderTopColor: s.color } as React.CSSProperties}>
              <div className="flex items-center justify-between mb-2">
                <span style={{ fontSize: '0.6rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.14em' }}>{s.label}</span>
                <s.icon size={12} style={{ color: s.color }} />
              </div>
              <div className="vs-stat-value tabular-nums" style={{ fontSize: '1.6rem', color: s.color }}>{s.value}</div>
            </div>
          ))}
        </div>

        {/* ── Charts row ── */}
        <div className="grid grid-cols-1 lg:grid-cols-5 gap-4">
          {/* Activity area chart */}
          <Card className="lg:col-span-3">
            <CardHeader title="Kill Activity (24h)" icon={Activity} color="text-cyan-400" />
            <div className="p-5">
              <ResponsiveContainer width="100%" height={130}>
                <AreaChart data={activityData} margin={{ left: -10, right: 4, top: 4, bottom: 0 }}>
                  <defs>
                    <linearGradient id="killBlueGrad" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%"  stopColor="#3b82f6" stopOpacity={0.3} />
                      <stop offset="95%" stopColor="#3b82f6" stopOpacity={0} />
                    </linearGradient>
                    <linearGradient id="killRedGrad" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%"  stopColor="#ef4444" stopOpacity={0.3} />
                      <stop offset="95%" stopColor="#ef4444" stopOpacity={0} />
                    </linearGradient>
                  </defs>
                  <XAxis dataKey="hour" tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} interval={3} />
                  <YAxis tick={{ fill: '#374151', fontSize: 11 }} axisLine={false} tickLine={false} />
                  <Tooltip {...TT} />
                  <Area type="monotone" dataKey="blue" name="Blue" stroke="#3b82f6" strokeWidth={1.5} fill="url(#killBlueGrad)" dot={false} />
                  <Area type="monotone" dataKey="red"  name="Red"  stroke="#ef4444" strokeWidth={1.5} fill="url(#killRedGrad)"  dot={false} />
                </AreaChart>
              </ResponsiveContainer>
            </div>
          </Card>

          {/* Top weapons */}
          <Card className="lg:col-span-2">
            <CardHeader title="Top Weapons" icon={Zap} color="text-orange-400" />
            <div className="p-5">
              {topWeapons.length > 0 ? (
                <ResponsiveContainer width="100%" height={130}>
                  <BarChart data={topWeapons} layout="vertical" margin={{ left: 0, right: 8 }}>
                    <XAxis type="number" tick={{ fill: '#374151', fontSize: 11 }} axisLine={false} tickLine={false} />
                    <YAxis type="category" dataKey="name" tick={{ fill: '#94a3b8', fontSize: 10 }} width={140} axisLine={false} tickLine={false} />
                    <Tooltip {...TT} />
                    <Bar dataKey="count" fill="#f97316" radius={[0, 3, 3, 0]} />
                  </BarChart>
                </ResponsiveContainer>
              ) : (
                <div className="h-[130px] flex items-center justify-center text-slate-700 text-sm">No data</div>
              )}
            </div>
          </Card>
        </div>

        {/* ── Kill feed table ── */}
        <Card>
          <CardHeader
            title="Event Log"
            icon={Crosshair}
            color="text-red-400"
            right={<span className="text-[10px] text-slate-700 font-mono">{kills.length} events</span>}
          />
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead className="border-b border-[#2a2a2a]">
                <tr>
                  {['Time', 'Type', 'Killer', 'Weapon', 'Target', 'Victim Side'].map(h => (
                    <th key={h} className="px-4 py-3 text-left text-[11px] uppercase tracking-widest text-slate-700 whitespace-nowrap">{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {isLoading && (
                  <tr><td colSpan={6} className="text-center py-10 text-slate-700 text-sm">Loading…</td></tr>
                )}
                {kills.length === 0 && !isLoading && (
                  <tr><td colSpan={6} className="text-center py-10 text-slate-700 text-sm">No kills recorded yet</td></tr>
                )}
                {kills.map((k, i) => {
                  const killerName = k.killer?.ucid ? (nameMap.get(k.killer.ucid) ?? 'Unknown Pilot') : null
                  const victimName = k.victim.ucid ? (nameMap.get(k.victim.ucid) ?? 'Unknown Pilot') : null
                  const kt = classifyKill(k.target_type, k.killer?.weapon ?? null)
                  return (
                    <tr key={i} className="border-b border-[#2a2a2a] kill-row">
                      <td className="px-4 py-2.5 text-[11px] font-mono text-slate-600 whitespace-nowrap tabular-nums">
                        {new Date(k.time).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' })}
                      </td>
                      <td className="px-4 py-2.5">
                        <span className="text-[10px] font-bold px-1.5 py-0.5 rounded" style={{ color: kt.color, background: `${kt.color}18` }}>
                          {kt.label}
                        </span>
                      </td>
                      <td className="px-4 py-2.5">
                        <div className="flex items-center gap-2">
                          <SideBadge side={k.killer?.side ?? 'Neutral'} size="xs" />
                          <span className="text-[12px] text-slate-200 font-semibold whitespace-nowrap">
                            {killerName ?? <span className="text-slate-600 font-normal">AI</span>}
                          </span>
                        </div>
                      </td>
                      <td className="px-4 py-2.5">
                        <span className="text-[11px] font-mono text-yellow-500/80 whitespace-nowrap">
                          {k.killer?.weapon ?? <span className="text-slate-700">—</span>}
                        </span>
                      </td>
                      <td className="px-4 py-2.5 text-[12px] text-slate-400 whitespace-nowrap">
                        {k.target_type ?? '—'}
                      </td>
                      <td className="px-4 py-2.5">
                        <div className="flex items-center gap-2">
                          <SideBadge side={k.victim.side} size="xs" />
                          {victimName && <span className="text-[11px] text-slate-400">{victimName}</span>}
                        </div>
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
          </div>
        </Card>
      </div>
    </div>
  )
}
