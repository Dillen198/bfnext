import React from 'react'
import { NavLink, Outlet, useNavigate } from 'react-router-dom'
import { LayoutDashboard, Map, Target, BarChart3, Users, Crosshair, WifiOff, Zap, LogOut, Shield, Settings, Info } from 'lucide-react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { useRound } from '../context/RoundContext'
import { useAuth } from '../context/AuthContext'
import { campaign } from '../config/campaign'

const nav = [
  { to: '/',            icon: LayoutDashboard, label: 'SITREP' },
  { to: '/map',         icon: Map,             label: 'TACMAP' },
  { to: '/objectives',  icon: Target,          label: 'OBJECTIVES' },
  { to: '/leaderboard', icon: BarChart3,        label: 'RANKINGS' },
  { to: '/pilots',      icon: Users,           label: 'PILOTS' },
  { to: '/kills',       icon: Crosshair,       label: 'KILL FEED' },
  { to: '/about',       icon: Info,            label: 'ABOUT' },
]

function Clock() {
  const [time, setTime] = React.useState(new Date())
  React.useEffect(() => {
    const t = setInterval(() => setTime(new Date()), 1000)
    return () => clearInterval(t)
  }, [])
  const d = time.toUTCString()
  return (
    <div className="font-mono-vs tabular-nums">
      <div className="text-[15px] font-bold text-white tracking-wider">{d.slice(17, 25)}</div>
      <div className="text-[10px] tracking-widest uppercase" style={{ color: 'var(--text-dim)' }}>{d.slice(0, 16)} UTC</div>
    </div>
  )
}

export default function Layout() {
  const { user, logout } = useAuth()
  const navigate = useNavigate()
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
  const { data: rounds = [] } = useQuery({
    queryKey: ['rounds'],
    queryFn: api.rounds,
    refetchInterval: 60_000,
  })

  const { selectedRound, setSelectedRound } = useRound()

  const blueCount = objectives.filter(o => o.owner === 'Blue').length
  const redCount  = objectives.filter(o => o.owner === 'Red').length
  const total     = objectives.length
  const bluePct   = total > 0 ? Math.round(blueCount / total * 100) : 0
  const redPct    = total > 0 ? Math.round(redCount  / total * 100) : 0

  const isLive     = !!stats?.active_round
  const activeRound = rounds.find(r => r.active)
  const pastRounds  = rounds.filter(r => !r.active)

  function roundLabel(r: { id: number; scenario: string; start: string; end: string | null; active: boolean }) {
    if (r.active) return `${r.scenario} (Active)`
    const start = new Date(r.start).toLocaleDateString()
    const end   = r.end ? new Date(r.end).toLocaleDateString() : '?'
    return `${r.scenario} · ${start}–${end}`
  }

  return (
    <div className="flex h-screen overflow-hidden" style={{ background: 'var(--bg)' }}>

      {/* ── Sidebar ── */}
      <aside
        className="w-64 flex-shrink-0 flex flex-col"
        style={{ background: '#0d0d0d', borderRight: '1px solid var(--border)' }}
      >

        {/* Logo */}
        <div className="px-4 pt-5 pb-4" style={{ borderBottom: '1px solid var(--border)' }}>
          <div className="flex items-center gap-3">
            {/* VS monogram */}
            <div
              className="flex items-center justify-center w-9 h-9 flex-shrink-0"
              style={{
                background: 'var(--accent)',
                borderRadius: '2px',
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1.1rem',
                letterSpacing: '0.05em',
                color: '#fff',
                lineHeight: 1,
              }}
            >
              VS
            </div>
            <div>
              <div
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1.05rem',
                  letterSpacing: '0.18em',
                  color: 'var(--text)',
                  lineHeight: 1,
                }}
              >
                {campaign.name}
              </div>
              <div
                className="mt-0.5"
                style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}
              >
                Operations Dashboard
              </div>
            </div>
          </div>
        </div>

        {/* Status */}
        <div className="px-4 py-3" style={{ borderBottom: '1px solid var(--border)' }}>
          <div className="flex items-center justify-between mb-2">
            <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.16em', textTransform: 'uppercase' }}>
              Mission Status
            </span>
            {isLive ? (
              <span className="vs-badge vs-badge-live">
                <span className="w-1.5 h-1.5 rounded-full bg-green-400 vs-pulse" />
                LIVE
              </span>
            ) : (
              <span className="vs-badge vs-badge-offline">
                <WifiOff size={8} />
                OFFLINE
              </span>
            )}
          </div>

          {stats?.active_round && (
            <div
              className="mb-2 truncate font-semibold"
              style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}
            >
              {stats.active_round.scenario}
            </div>
          )}

          <Clock />
        </div>

        {/* Round selector */}
        {rounds.length > 0 && (
          <div className="px-4 py-3" style={{ borderBottom: '1px solid var(--border)' }}>
            <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.16em', textTransform: 'uppercase', marginBottom: '0.4rem' }}>
              Viewing Round
            </div>
            <select
              value={selectedRound ?? ''}
              onChange={e => setSelectedRound(e.target.value === '' ? undefined : Number(e.target.value))}
              className="vs-input w-full cursor-pointer"
            >
              {activeRound && <option value="">{roundLabel(activeRound)}</option>}
              {!activeRound && <option value="">Latest Round</option>}
              {pastRounds.map(r => (
                <option key={r.id} value={r.id}>{roundLabel(r)}</option>
              ))}
            </select>
            {selectedRound !== undefined && (
              <button
                onClick={() => setSelectedRound(undefined)}
                className="mt-1.5 transition-colors"
                style={{ fontSize: '0.65rem', color: 'var(--accent)', background: 'none', border: 'none', cursor: 'pointer', padding: 0 }}
              >
                ← Back to active
              </button>
            )}
          </div>
        )}

        {/* Territory bar */}
        {total > 0 && (
          <div className="px-4 py-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
            <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.16em', textTransform: 'uppercase', marginBottom: '0.4rem' }}>
              Territory Control
            </div>
            <div className="h-1.5 rounded-none overflow-hidden flex" style={{ background: 'rgba(75,85,99,0.15)' }}>
              <div className="h-full health-fill" style={{ width: `${bluePct}%`, background: 'var(--blue)' }} />
              <div className="h-full health-fill" style={{ width: `${redPct}%`, background: 'var(--accent)' }} />
            </div>
            <div className="flex justify-between mt-1">
              <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--blue)' }}>{bluePct}% {campaign.blueLabel}</span>
              <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--accent)' }}>{campaign.redLabel} {redPct}%</span>
            </div>
          </div>
        )}

        {/* Nav */}
        <nav className="flex-1 py-2 px-2 space-y-0.5 overflow-y-auto">
          {[...nav, ...(user?.is_admin ? [{ to: '/admin', icon: Settings, label: 'ADMIN' }] : [])].map(({ to, icon: Icon, label }) => (
            <NavLink
              key={to}
              to={to}
              end={to === '/'}
              className={({ isActive }) =>
                `flex items-center gap-3 px-3 py-2.5 transition-all duration-150 ${
                  isActive ? 'active-nav-link' : 'inactive-nav-link'
                }`
              }
              style={({ isActive }) => ({
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '0.8rem',
                letterSpacing: '0.14em',
                borderRadius: '2px',
                borderLeft: isActive ? '3px solid var(--accent)' : '3px solid transparent',
                paddingLeft: isActive ? '10px' : '12px',
                color: isActive ? 'var(--text)' : 'var(--text-dim)',
                background: isActive ? 'rgba(77,124,15,0.07)' : 'transparent',
                display: 'flex',
                alignItems: 'center',
                gap: '0.75rem',
                textDecoration: 'none',
              })}
            >
              <Icon size={13} className="flex-shrink-0" />
              {label}
            </NavLink>
          ))}
        </nav>

        {/* Quick stats */}
        {stats && (
          <div className="px-4 py-3" style={{ borderTop: '1px solid var(--border)' }}>
            <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.16em', textTransform: 'uppercase', marginBottom: '0.5rem' }}>
              Quick Stats
            </div>
            <div className="space-y-1.5">
              {[
                { label: 'Active Pilots', value: stats.total_pilots,    color: 'var(--blue)' },
                { label: 'Objectives',    value: stats.objective_count, color: '#f59e0b' },
                { label: 'Total Kills',   value: stats.total_kills,     color: 'var(--accent)' },
                { label: 'Rounds',        value: stats.total_rounds,    color: 'var(--text-muted)' },
              ].map(s => (
                <div key={s.label} className="flex justify-between items-center">
                  <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)' }}>{s.label}</span>
                  <span className="font-mono-vs font-bold" style={{ fontSize: '0.8rem', color: s.color }}>{s.value}</span>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* User / login */}
        <div className="px-4 py-3" style={{ borderTop: '1px solid var(--border)' }}>
          {user ? (
            <div>
              <div className="flex items-center gap-2 mb-2">
                {user.avatar ? (
                  <img
                    src={`https://cdn.discordapp.com/avatars/${user.discord_id}/${user.avatar}.webp?size=32`}
                    alt=""
                    style={{ width: 24, height: 24, borderRadius: '50%' }}
                  />
                ) : (
                  <div style={{ width: 24, height: 24, borderRadius: '50%', background: 'var(--accent)', flexShrink: 0 }} />
                )}
                <div style={{ minWidth: 0 }}>
                  <div className="truncate" style={{ fontSize: '0.68rem', color: 'var(--text)', fontWeight: 600 }}>
                    {user.username}
                  </div>
                  {user.is_admin && (
                    <div className="flex items-center gap-1" style={{ fontSize: '0.55rem', color: '#f59e0b', letterSpacing: '0.1em' }}>
                      <Shield size={8} /> ADMIN
                    </div>
                  )}
                </div>
              </div>
              <button
                onClick={logout}
                className="flex items-center gap-1.5 transition-colors"
                style={{
                  fontSize: '0.62rem', color: 'var(--text-dim)', background: 'none',
                  border: 'none', cursor: 'pointer', padding: 0, letterSpacing: '0.1em',
                }}
              >
                <LogOut size={9} /> LOGOUT
              </button>
            </div>
          ) : (
            <button
              onClick={() => navigate('/login')}
              style={{
                display: 'flex', alignItems: 'center', gap: '0.5rem',
                fontSize: '0.65rem', color: '#5865F2', background: 'none',
                border: 'none', cursor: 'pointer', padding: 0,
                letterSpacing: '0.1em',
                fontFamily: "'Bebas Neue', sans-serif",
              }}
            >
              <svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
                <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
              </svg>
              LOGIN WITH DISCORD
            </button>
          )}
        </div>

        {/* Footer */}
        <div className="px-4 py-2" style={{ borderTop: '1px solid var(--border)' }}>
          <div className="flex items-center gap-1.5 mb-1">
            <Zap size={9} style={{ color: 'var(--accent)', opacity: 0.6 }} />
            <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textTransform: 'uppercase' }}>
              {campaign.name} · {campaign.version}
            </span>
          </div>
          <div style={{ fontSize: '0.54rem', color: 'var(--text-dim)', letterSpacing: '0.08em', opacity: 0.6 }}>
            Developed &amp; Customized by No15 | KillerDog &amp; [.ID] EagleEye
          </div>
        </div>
      </aside>

      {/* ── Main content ── */}
      <main className="flex-1 overflow-hidden flex flex-col min-w-0">
        <Outlet />
      </main>
    </div>
  )
}
