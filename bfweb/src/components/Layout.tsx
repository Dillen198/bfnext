import React from 'react'
import { NavLink, Outlet } from 'react-router-dom'
import { LayoutDashboard, Map, Target, BarChart3, Users, Crosshair, Radio, Activity, WifiOff } from 'lucide-react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'

const nav = [
  { to: '/', icon: LayoutDashboard, label: 'SITREP' },
  { to: '/map', icon: Map, label: 'TACMAP' },
  { to: '/objectives', icon: Target, label: 'OBJECTIVES' },
  { to: '/leaderboard', icon: BarChart3, label: 'RANKINGS' },
  { to: '/pilots', icon: Users, label: 'PILOTS' },
  { to: '/kills', icon: Crosshair, label: 'KILL FEED' },
]

function Clock() {
  const [time, setTime] = React.useState(new Date())
  React.useEffect(() => {
    const t = setInterval(() => setTime(new Date()), 1000)
    return () => clearInterval(t)
  }, [])
  const d = time.toUTCString()
  return (
    <div className="font-mono tabular-nums">
      <div className="text-[13px] font-semibold text-slate-200 tracking-wider">{d.slice(17, 25)}</div>
      <div className="text-[9px] text-slate-600 tracking-widest uppercase">{d.slice(0, 16)} UTC</div>
    </div>
  )
}

export default function Layout() {
  const { data: stats } = useQuery({
    queryKey: ['stats'],
    queryFn: api.stats,
    refetchInterval: 30_000,
  })
  const { data: objectives = [] } = useQuery({
    queryKey: ['objectives'],
    queryFn: () => api.objectives(),
    refetchInterval: 30_000,
  })

  const blueCount = objectives.filter(o => o.owner === 'Blue').length
  const redCount = objectives.filter(o => o.owner === 'Red').length
  const total = objectives.length
  const bluePct = total > 0 ? Math.round(blueCount / total * 100) : 0
  const redPct  = total > 0 ? Math.round(redCount  / total * 100) : 0

  const isLive = !!stats?.active_round

  return (
    <div className="flex h-screen overflow-hidden bg-[#020810]">
      {/* Sidebar */}
      <aside className="w-56 flex-shrink-0 flex flex-col border-r border-[#1e3a5f]/50" style={{ background: 'linear-gradient(180deg, #040c18 0%, #030a14 100%)' }}>
        {/* Logo */}
        <div className="px-4 pt-5 pb-4 border-b border-[#1e3a5f]/40">
          <div className="flex items-center gap-2.5 mb-1">
            <div className="relative w-7 h-7 flex items-center justify-center rounded">
              <div className="absolute inset-0 bg-blue-500/15 rounded border border-blue-500/30" />
              <Radio size={13} className="text-blue-400 relative z-10" />
            </div>
            <div>
              <div className="text-[13px] font-bold tracking-[0.25em] text-blue-300">FOWL</div>
              <div className="text-[8px] text-slate-700 tracking-[0.2em] uppercase -mt-0.5">Campaign Engine</div>
            </div>
          </div>
        </div>

        {/* Campaign status */}
        <div className="px-4 py-3 border-b border-[#1e3a5f]/40">
          <div className="flex items-center justify-between mb-2">
            <span className="text-[9px] text-slate-700 uppercase tracking-widest">Status</span>
            {isLive ? (
              <span className="status-live">
                <span className="w-1.5 h-1.5 rounded-full bg-green-400 glow-pulse" />
                LIVE
              </span>
            ) : (
              <span className="status-offline">
                <WifiOff size={9} />
                OFFLINE
              </span>
            )}
          </div>

          {stats?.active_round && (
            <div className="text-[10px] text-slate-400 font-semibold truncate mb-1">{stats.active_round.scenario}</div>
          )}

          <Clock />
        </div>

        {/* Territory mini bar */}
        {total > 0 && (
          <div className="px-4 py-2.5 border-b border-[#1e3a5f]/40">
            <div className="text-[9px] text-slate-700 uppercase tracking-widest mb-1.5">Territory</div>
            <div className="h-1.5 rounded-full overflow-hidden flex bg-[#4b5563]/20">
              <div className="h-full bg-blue-500 transition-all duration-500" style={{ width: `${bluePct}%` }} />
              <div className="h-full bg-red-500 transition-all duration-500" style={{ width: `${redPct}%` }} />
            </div>
            <div className="flex justify-between mt-1">
              <span className="text-[9px] font-mono text-blue-400">{bluePct}% BLU</span>
              <span className="text-[9px] font-mono text-red-400">RED {redPct}%</span>
            </div>
          </div>
        )}

        {/* Nav */}
        <nav className="flex-1 py-2 px-2 space-y-0.5">
          {nav.map(({ to, icon: Icon, label }) => (
            <NavLink
              key={to}
              to={to}
              end={to === '/'}
              className={({ isActive }) =>
                `flex items-center gap-3 px-3 py-2 rounded text-[11px] font-semibold tracking-widest transition-all duration-150 ${
                  isActive
                    ? 'bg-blue-500/10 text-blue-300 border-l-2 border-blue-400 pl-[10px]'
                    : 'text-slate-600 hover:text-slate-300 hover:bg-white/[0.03] border-l-2 border-transparent'
                }`
              }
            >
              <Icon size={12} className="flex-shrink-0" />
              {label}
            </NavLink>
          ))}
        </nav>

        {/* Bottom stats panel */}
        {stats && (
          <div className="px-4 py-3 border-t border-[#1e3a5f]/40 space-y-2">
            <div className="text-[9px] text-slate-700 uppercase tracking-widest mb-1.5">Quick Stats</div>
            {[
              { label: 'Pilots', value: stats.total_pilots, color: 'text-blue-400' },
              { label: 'Objectives', value: stats.objective_count, color: 'text-yellow-500' },
              { label: 'Total Kills', value: stats.total_kills, color: 'text-red-400' },
              { label: 'Rounds', value: stats.total_rounds, color: 'text-slate-400' },
            ].map(s => (
              <div key={s.label} className="flex justify-between items-center">
                <span className="text-[10px] text-slate-600">{s.label}</span>
                <span className={`text-[11px] font-mono font-bold ${s.color}`}>{s.value}</span>
              </div>
            ))}
          </div>
        )}

        {/* Version footer */}
        <div className="px-4 py-2 border-t border-[#1e3a5f]/30">
          <div className="flex items-center gap-1.5">
            <Activity size={9} className="text-slate-700" />
            <span className="text-[9px] text-slate-700 tracking-wider">FOWL ENGINE · v2</span>
          </div>
        </div>
      </aside>

      {/* Main content */}
      <main className="flex-1 overflow-hidden flex flex-col min-w-0">
        <Outlet />
      </main>
    </div>
  )
}
