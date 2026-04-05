import React, { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import {
  RadarChart, Radar, PolarGrid, PolarAngleAxis, ResponsiveContainer, Tooltip,
  BarChart, Bar, XAxis, YAxis, Cell,
} from 'recharts'
import PageHeader from '../components/PageHeader'
import { Users, Crosshair, Clock, Award, TrendingUp, Shield } from 'lucide-react'

const TT = {
  contentStyle: { background: '#0a1628', border: '1px solid #1a3555', borderRadius: 6, color: '#c9d1d9', fontSize: 12 },
  cursor: { fill: 'rgba(59,130,246,0.05)' },
}

function kd(air: number, ground: number, deaths: number) {
  const k = air + ground
  return deaths > 0 ? (k / deaths).toFixed(2) : k > 0 ? '∞' : '0.00'
}

function computeScore(air: number, ground: number, captures: number, repairs: number,
  supply: number, troops: number, farps: number, deploys: number, actions: number, deaths: number) {
  return Math.round((air * 3) + (ground * 2) + (captures * 5) + repairs + supply +
    (troops * 0.5) + (farps * 4) + (deploys * 2) + actions - (deaths * 2))
}

const STAT_COLORS: Record<string, string> = {
  'Air Kills': '#3b82f6', 'Ground':   '#f97316', 'Captures': '#eab308',
  'Repairs':   '#22c55e', 'Supply':   '#06b6d4', 'Troops':   '#8b5cf6',
  'FARPs':     '#ec4899', 'Actions':  '#94a3b8',
}

export default function Pilots() {
  const { data: pilots = [], isLoading } = useQuery({
    queryKey: ['leaderboard'],
    queryFn: api.leaderboard,
    refetchInterval: 60_000,
  })
  const [selected, setSelected] = useState<string | null>(null)
  const [search, setSearch] = useState('')

  const filtered = pilots.filter(p => !search || p.name.toLowerCase().includes(search.toLowerCase()))
  const pilot = pilots.find(p => p.ucid === selected)

  const globalMaxes = {
    air_kills:        Math.max(...pilots.map(p => p.air_kills), 1),
    ground_kills:     Math.max(...pilots.map(p => p.ground_kills), 1),
    captures:         Math.max(...pilots.map(p => p.captures), 1),
    repairs:          Math.max(...pilots.map(p => p.repairs), 1),
    supply_transfers: Math.max(...pilots.map(p => p.supply_transfers), 1),
    troops:           Math.max(...pilots.map(p => p.troops), 1),
  }

  const radarData = pilot ? [
    { stat: 'Air Kills',  value: pilot.air_kills,        fullMark: globalMaxes.air_kills },
    { stat: 'Ground',     value: pilot.ground_kills,     fullMark: globalMaxes.ground_kills },
    { stat: 'Captures',   value: pilot.captures,         fullMark: globalMaxes.captures },
    { stat: 'Repairs',    value: pilot.repairs,          fullMark: globalMaxes.repairs },
    { stat: 'Supply',     value: pilot.supply_transfers, fullMark: globalMaxes.supply_transfers },
    { stat: 'Troops',     value: pilot.troops,           fullMark: globalMaxes.troops },
  ] : []

  const barData = pilot ? [
    { name: 'Air Kills', value: pilot.air_kills },
    { name: 'Ground',    value: pilot.ground_kills },
    { name: 'Captures',  value: pilot.captures },
    { name: 'Repairs',   value: pilot.repairs },
    { name: 'Supply',    value: pilot.supply_transfers },
    { name: 'Troops',    value: pilot.troops },
    { name: 'FARPs',     value: pilot.farps },
    { name: 'Actions',   value: pilot.actions },
  ] : []

  // Pilot rank among all
  const pilotRank = pilot ? pilots.indexOf(pilots.find(p => p.ucid === pilot.ucid)!) + 1 : null

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader title="PILOTS" sub={`${pilots.length} registered pilots`} />

      <div className="flex-1 overflow-hidden flex min-h-0">
        {/* ── Pilot list sidebar ── */}
        <div className="w-56 flex-shrink-0 border-r border-[#1e3a5f]/50 flex flex-col" style={{ background: '#030a14' }}>
          <div className="p-3 border-b border-[#1e3a5f]/40">
            <input
              type="text"
              placeholder="Search pilots…"
              value={search}
              onChange={e => setSearch(e.target.value)}
              className="w-full bg-[#050d1a] border border-[#1e3a5f]/60 rounded px-3 py-1.5 text-[11px] text-slate-300 placeholder:text-slate-700 focus:outline-none focus:border-blue-500/50"
            />
          </div>
          <div className="flex-1 overflow-y-auto">
            {isLoading && <div className="text-center py-8 text-slate-700 text-xs">Loading…</div>}
            {filtered.map((p, i) => {
              const totalKills = p.air_kills + p.ground_kills
              const isSelected = selected === p.ucid
              return (
                <button
                  key={p.ucid}
                  onClick={() => setSelected(p.ucid)}
                  className={`w-full flex items-center gap-2.5 px-3 py-2.5 text-left border-b border-[#0a1520] transition-colors ${
                    isSelected ? 'bg-blue-500/10 border-l-2 border-l-blue-400 !pl-[10px]' : 'hover:bg-blue-500/[0.04]'
                  }`}
                >
                  <span className="text-[10px] text-slate-700 w-5 shrink-0 font-mono">{i + 1}</span>
                  <div className="flex-1 min-w-0">
                    <div className="text-[11px] font-semibold text-slate-100 truncate">{p.name}</div>
                    <div className="text-[9px] text-slate-600 mt-0.5 font-mono">
                      {totalKills}k · {p.hours.toFixed(1)}h · {p.deaths}d
                    </div>
                  </div>
                </button>
              )
            })}
            {!isLoading && filtered.length === 0 && (
              <div className="text-center py-8 text-slate-700 text-xs">No pilots found</div>
            )}
          </div>
        </div>

        {/* ── Detail panel ── */}
        <div className="flex-1 overflow-auto p-4 grid-bg">
          {!pilot ? (
            <div className="flex flex-col items-center justify-center h-full text-slate-700 gap-3">
              <Users size={36} className="opacity-20" />
              <div className="text-[11px] tracking-widest uppercase">Select a pilot to view details</div>
            </div>
          ) : (
            <div className="space-y-4 max-w-5xl">
              {/* ── Pilot header card ── */}
              <div className="tac-card px-5 py-4">
                <div className="flex items-center gap-5">
                  {/* Avatar */}
                  <div className="w-14 h-14 rounded-lg bg-blue-500/10 border border-blue-500/20 flex items-center justify-center text-blue-300 font-bold text-xl flex-shrink-0">
                    {pilot.name[0]?.toUpperCase()}
                  </div>
                  {/* Name + rank */}
                  <div className="flex-1 min-w-0">
                    <div className="text-[18px] font-bold text-slate-50">{pilot.name}</div>
                    <div className="flex items-center gap-2 mt-0.5">
                      {pilotRank && pilotRank <= 3 && (
                        <span className="text-lg">{['🥇', '🥈', '🥉'][pilotRank - 1]}</span>
                      )}
                      {pilotRank && (
                        <span className="text-[10px] text-slate-600 font-mono">Rank #{pilotRank}</span>
                      )}
                      <span className="text-[10px] text-slate-700 font-mono">· {pilot.ucid.slice(0, 12)}…</span>
                    </div>
                  </div>
                  {/* Key stats */}
                  <div className="flex gap-6 shrink-0">
                    {[
                      { label: 'K/D', value: kd(pilot.air_kills, pilot.ground_kills, pilot.deaths), color: 'text-green-400', icon: TrendingUp },
                      { label: 'Kills', value: pilot.air_kills + pilot.ground_kills, color: 'text-red-400', icon: Crosshair },
                      { label: 'Hours', value: `${pilot.hours.toFixed(1)}h`, color: 'text-blue-300', icon: Clock },
                      { label: 'Score', value: computeScore(pilot.air_kills, pilot.ground_kills, pilot.captures, pilot.repairs, pilot.supply_transfers, pilot.troops, pilot.farps, pilot.deploys, pilot.actions, pilot.deaths), color: 'text-purple-400', icon: Award },
                    ].map(s => (
                      <div key={s.label} className="text-center">
                        <s.icon size={12} className={`${s.color} mx-auto mb-1`} />
                        <div className={`text-[20px] leading-none font-bold font-mono ${s.color}`}>{s.value}</div>
                        <div className="text-[9px] text-slate-700 uppercase tracking-wider mt-0.5">{s.label}</div>
                      </div>
                    ))}
                  </div>
                </div>
              </div>

              {/* ── Charts ── */}
              <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
                <div className="tac-card p-4">
                  <div className="text-[10px] text-slate-600 uppercase tracking-widest mb-3 flex items-center gap-1.5">
                    <Shield size={10} className="text-blue-400" />
                    Performance Profile
                  </div>
                  <ResponsiveContainer width="100%" height={240}>
                    <RadarChart data={radarData}>
                      <PolarGrid stroke="#1a3355" />
                      <PolarAngleAxis dataKey="stat" tick={{ fill: '#475569', fontSize: 10 }} />
                      <Radar dataKey="value" stroke="#3b82f6" fill="#3b82f6" fillOpacity={0.15} strokeWidth={2} />
                      <Tooltip {...TT} />
                    </RadarChart>
                  </ResponsiveContainer>
                </div>

                <div className="tac-card p-4">
                  <div className="text-[10px] text-slate-600 uppercase tracking-widest mb-3 flex items-center gap-1.5">
                    <TrendingUp size={10} className="text-green-400" />
                    Stat Breakdown
                  </div>
                  <ResponsiveContainer width="100%" height={240}>
                    <BarChart data={barData} margin={{ left: -10, right: 8, top: 4, bottom: 20 }}>
                      <XAxis dataKey="name" tick={{ fill: '#374151', fontSize: 9 }} axisLine={false} tickLine={false} angle={-30} textAnchor="end" />
                      <YAxis tick={{ fill: '#374151', fontSize: 10 }} axisLine={false} tickLine={false} width={25} />
                      <Tooltip {...TT} />
                      <Bar dataKey="value" radius={[3, 3, 0, 0]}>
                        {barData.map((d, i) => (
                          <Cell key={i} fill={STAT_COLORS[d.name] ?? '#3b82f6'} />
                        ))}
                      </Bar>
                    </BarChart>
                  </ResponsiveContainer>
                </div>
              </div>

              {/* ── Stat grid ── */}
              <div className="grid grid-cols-3 sm:grid-cols-4 lg:grid-cols-6 gap-2">
                {[
                  { label: 'Air Kills',   value: pilot.air_kills,          color: 'text-blue-400',   border: '#3b82f622' },
                  { label: 'Ground Kills',value: pilot.ground_kills,       color: 'text-orange-400', border: '#f9731622' },
                  { label: 'Deaths',      value: pilot.deaths,             color: 'text-red-400',    border: '#ef444422' },
                  { label: 'K/D Ratio',   value: kd(pilot.air_kills, pilot.ground_kills, pilot.deaths), color: 'text-green-400', border: '#22c55e22' },
                  { label: 'Captures',    value: pilot.captures,           color: 'text-yellow-400', border: '#fbbf2422' },
                  { label: 'Score',       value: computeScore(pilot.air_kills, pilot.ground_kills, pilot.captures, pilot.repairs, pilot.supply_transfers, pilot.troops, pilot.farps, pilot.deploys, pilot.actions, pilot.deaths), color: 'text-purple-400', border: '#a78bfa22' },
                  { label: 'Repairs',     value: pilot.repairs,            color: 'text-green-400',  border: '#22c55e22' },
                  { label: 'Supply',      value: pilot.supply_transfers,   color: 'text-cyan-400',   border: '#06b6d422' },
                  { label: 'Troops',      value: pilot.troops,             color: 'text-violet-400', border: '#8b5cf622' },
                  { label: 'FARPs',       value: pilot.farps,              color: 'text-pink-400',   border: '#ec489922' },
                  { label: 'Deploys',     value: pilot.deploys,            color: 'text-slate-300',  border: '#94a3b822' },
                  { label: 'Flight Hrs',  value: `${pilot.hours.toFixed(1)}h`, color: 'text-blue-300', border: '#60a5fa22' },
                ].map(s => (
                  <div
                    key={s.label}
                    className="tac-card px-3 py-2.5"
                    style={{ borderColor: s.border } as React.CSSProperties}
                  >
                    <div className="absolute inset-x-0 top-0 h-px" style={{ background: `linear-gradient(to right,transparent,${s.border.replace('22', '66')},transparent)` }} />
                    <div className="text-[8px] text-slate-700 uppercase tracking-widest mb-1">{s.label}</div>
                    <div className={`text-[16px] leading-none font-bold font-mono ${s.color}`}>{s.value}</div>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
