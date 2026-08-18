import React from 'react'
import { NavLink, Outlet, useNavigate } from 'react-router-dom'
import {
  LayoutDashboard, Map, Target, BarChart3, Users, Crosshair,
  Zap, LogOut, Shield, Settings, Settings2, Info, Server, Radio,
  ChevronRight, Plane, Menu, X,
} from 'lucide-react'
import { useQuery } from '@tanstack/react-query'
import { api, type Weather } from '../api'
import { useRound } from '../context/RoundContext'
import { useAuth } from '../context/AuthContext'
import { campaign } from '../config/campaign'

// ── Nav config ────────────────────────────────────────────────────────────────

const NAV = [
  { to: '/',            icon: LayoutDashboard, label: 'SITREP'     },
  { to: '/map',         icon: Map,             label: 'TACMAP'     },
  { to: '/objectives',  icon: Target,          label: 'OBJECTIVES' },
  { to: '/leaderboard', icon: BarChart3,        label: 'RANKINGS'   },
  { to: '/pilots',      icon: Users,           label: 'PILOTS'     },
  { to: '/kills',       icon: Crosshair,       label: 'KILL FEED'  },
  { to: '/about',       icon: Info,            label: 'ABOUT'      },
]
const ADMIN_NAV = { to: '/admin', icon: Settings, label: 'ADMIN' }
const CONFIG_NAV = { to: '/admin/config', icon: Settings2, label: 'CONFIG' }
const PROFILE_NAV = (ucid: string) => ({ to: `/pilot/${ucid}`, icon: Users, label: 'MY PROFILE' })

// ── Helpers ───────────────────────────────────────────────────────────────────

function pad(n: number) { return String(n).padStart(2, '0') }
function fmtDuration(s: number) {
  return `${pad(Math.floor(s / 3600))}:${pad(Math.floor((s % 3600) / 60))}:${pad(Math.floor(s % 60))}`
}

// ── Clock ─────────────────────────────────────────────────────────────────────

function Clock() {
  const [now, setNow] = React.useState(new Date())
  React.useEffect(() => {
    const t = setInterval(() => setNow(new Date()), 1000)
    return () => clearInterval(t)
  }, [])
  const utc = now.toUTCString()
  return (
    <div style={{ textAlign: 'right', lineHeight: 1.2 }}>
      <div className="font-mono-vs tabular-nums" style={{ fontSize: '0.95rem', fontWeight: 700, color: 'var(--text)', letterSpacing: '0.05em' }}>
        {utc.slice(17, 25)} <span style={{ color: 'var(--accent)', fontSize: '0.62rem', letterSpacing: '0.1em' }}>Z</span>
      </div>
      <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.06em' }}>
        {utc.slice(5, 16)}
      </div>
    </div>
  )
}

function SessionTimer({ start }: { start: string }) {
  const [elapsed, setElapsed] = React.useState(0)
  React.useEffect(() => {
    const startMs = new Date(start).getTime()
    const tick = () => setElapsed(Math.max(0, Math.floor((Date.now() - startMs) / 1000)))
    tick(); const t = setInterval(tick, 1000); return () => clearInterval(t)
  }, [start])
  return (
    <div style={{ textAlign: 'right', lineHeight: 1.2 }}>
      <div className="font-mono-vs tabular-nums" style={{ fontSize: '0.9rem', fontWeight: 700, color: 'var(--text-muted)', letterSpacing: '0.05em' }}>
        {fmtDuration(elapsed)}
      </div>
      <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.08em', textTransform: 'uppercase' }}>Mission</div>
    </div>
  )
}

function RestartCountdown({ restartAt }: { restartAt: string }) {
  const [remaining, setRemaining] = React.useState(0)
  React.useEffect(() => {
    const endMs = new Date(restartAt).getTime()
    const tick = () => setRemaining(Math.max(0, Math.floor((endMs - Date.now()) / 1000)))
    tick(); const t = setInterval(tick, 1000); return () => clearInterval(t)
  }, [restartAt])
  const urgent = remaining < 1800
  return (
    <div style={{ textAlign: 'right', lineHeight: 1.2 }}>
      <div className="font-mono-vs tabular-nums" style={{ fontSize: '0.85rem', fontWeight: 700, color: urgent ? '#fbbf24' : 'var(--text-dim)', letterSpacing: '0.05em' }}>
        {fmtDuration(remaining)}
      </div>
      <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.08em', textTransform: 'uppercase' }}>Restart</div>
    </div>
  )
}

// ── Weather pill ──────────────────────────────────────────────────────────────

const COMPASS = ['N','NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W','WNW','NW','NNW']
const windDir = (deg: number) => COMPASS[Math.round(deg / 22.5) % 16]

type FlightRule = 'VFR' | 'MVFR' | 'IFR' | 'LIFR'
function flightRule(ceilingFt: number | null, visM: number | null): FlightRule {
  const visKm = visM !== null ? visM / 1000 : 99
  const ceil  = ceilingFt ?? 9999
  if (ceil < 500  || visKm < 1.6) return 'LIFR'
  if (ceil < 1000 || visKm < 4.8) return 'IFR'
  if (ceil < 3000 || visKm < 8)   return 'MVFR'
  return 'VFR'
}
const RULE_COLOR: Record<FlightRule, string> = {
  VFR: '#4ade80', MVFR: '#60a5fa', IFR: '#f87171', LIFR: '#c084fc',
}
const CLOUD: Record<number, string> = { 0:'SKC',1:'FEW',2:'FEW',3:'FEW',4:'FEW',5:'SCT',6:'SCT',7:'BKN',8:'OVC' }
const cloudCover = (d: number | null) => d === null ? 'CLR' : (CLOUD[Math.round(Math.max(0, Math.min(8, d)))] ?? 'CLR')

function WeatherPill({ w }: { w: Weather }) {
  const hasCeiling = (w.cloud_density ?? 0) >= 5
  const cloudFt    = Math.round(w.cloud_base_m * 3.281 / 100) * 100
  const rule       = flightRule(hasCeiling ? cloudFt : null, w.visibility_m ?? null)
  const color      = RULE_COLOR[rule]
  const cover      = cloudCover(w.cloud_density ?? null)
  const vis        = w.visibility_m !== null
    ? w.visibility_m >= 9999 ? '10+km' : `${(w.visibility_m / 1000).toFixed(1)}km`
    : null
  const cell = (label: string, val: string, c = 'var(--text-muted)') => (
    <div style={{ lineHeight: 1.2, textAlign: 'center' }}>
      <div style={{ fontSize: '0.52rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>{label}</div>
      <div className="font-mono-vs" style={{ fontSize: '0.72rem', color: c, fontWeight: 600 }}>{val}</div>
    </div>
  )
  const calm = w.wind_speed_kts < 1
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 10, flexShrink: 0 }}>
      <span style={{
        fontSize: '0.7rem', fontWeight: 800, letterSpacing: '0.1em',
        color, border: `1px solid ${color}44`, padding: '3px 8px', borderRadius: 4,
        background: `${color}10`, lineHeight: 1,
      }}>{rule}</span>
      <div style={{ width: 1, height: 26, background: 'var(--border-light)', flexShrink: 0 }} />
      {cell('WIND', calm ? 'CALM' : `${windDir(w.wind_from_deg)} ${Math.round(w.wind_speed_kts)}kt`)}
      {cell('TEMP', `${Math.round(w.temp_c)}°C`)}
      {cell('QNH',  `${Math.round(w.qnh_hpa)}`)}
      {cell('SKY',  hasCeiling ? `${cover} ${cloudFt}ft` : cover)}
      {vis && cell('VIS', vis)}
    </div>
  )
}

// ── Territory bar ─────────────────────────────────────────────────────────────

function TerritoryBar({ bluePct, redPct }: { bluePct: number; redPct: number }) {
  const neutPct = Math.max(0, 100 - bluePct - redPct)
  return (
    <div style={{ flexShrink: 0 }}>
      <div style={{ width: 90, height: 5, overflow: 'hidden', display: 'flex', background: 'var(--border-light)', borderRadius: 3 }}>
        <div style={{ width: `${bluePct}%`, background: 'var(--blue)',  transition: 'width 0.5s' }} />
        <div style={{ width: `${neutPct}%`, background: '#2d3748',     transition: 'width 0.5s' }} />
        <div style={{ width: `${redPct}%`,  background: 'var(--red)',   transition: 'width 0.5s' }} />
      </div>
      <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: 3 }}>
        <span className="font-mono-vs" style={{ fontSize: '0.62rem', color: 'var(--blue)', fontWeight: 700 }}>{bluePct}%</span>
        <span className="font-mono-vs" style={{ fontSize: '0.62rem', color: 'var(--red)',  fontWeight: 700 }}>{redPct}%</span>
      </div>
    </div>
  )
}

// ── Discord icon ──────────────────────────────────────────────────────────────

function DiscordIcon({ size = 11 }: { size?: number }) {
  return (
    <svg width={size} height={size} viewBox="0 0 24 24" fill="currentColor">
      <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
    </svg>
  )
}

// ── Main Layout ───────────────────────────────────────────────────────────────

export default function Layout() {
  const { user, logout } = useAuth()
  const navigate = useNavigate()
  const [sidebarOpen, setSidebarOpen] = React.useState(false)

  const { data: stats }       = useQuery({ queryKey: ['stats'],      queryFn: api.stats,      refetchInterval: 30_000 })
  const { data: objectives = [] } = useQuery({ queryKey: ['objectives'], queryFn: () => api.objectives(), refetchInterval: 30_000 })
  const { data: rounds = [] } = useQuery({ queryKey: ['rounds'],     queryFn: api.rounds,     refetchInterval: 60_000 })

  const { selectedRound, setSelectedRound } = useRound()

  const blueCount = objectives.filter(o => o.owner === 'Blue').length
  const redCount  = objectives.filter(o => o.owner === 'Red').length
  const total     = objectives.length
  const bluePct   = total > 0 ? Math.round(blueCount / total * 100) : 0
  const redPct    = total > 0 ? Math.round(redCount  / total * 100) : 0

  const isLive      = !!stats?.active_round
  const activeRound = rounds.find(r => r.active)
  const pastRounds  = rounds.filter(r => !r.active)

  const allNav = [
    ...NAV, 
    ...(user?.ucid ? [PROFILE_NAV(user.ucid)] : []),
    ...(user?.is_admin ? [ADMIN_NAV, CONFIG_NAV] : [])
  ]

  const Sep = () => <div className="topbar-sep" />

  function roundLabel(r: { id: number; scenario: string; start: string; end: string | null; active: boolean }) {
    if (r.active) return `${r.scenario} — Active`
    const start = new Date(r.start).toLocaleDateString([], { month: 'short', day: 'numeric' })
    const end   = r.end ? new Date(r.end).toLocaleDateString([], { month: 'short', day: 'numeric' }) : '?'
    return `${r.scenario} · ${start}–${end}`
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'column', height: '100vh', overflow: 'hidden', background: 'var(--bg)' }}>

      {/* ══════════════════════════════════════════════════════════
          TOP BAR
          ══════════════════════════════════════════════════════════ */}
      <header className="topbar" style={{ padding: '0 16px', gap: 0 }}>

        {/* Mobile nav toggle */}
        <button
          className="mobile-menu-btn"
          onClick={() => setSidebarOpen(v => !v)}
          aria-label={sidebarOpen ? 'Close navigation' : 'Open navigation'}
          style={{ marginRight: 12 }}
        >
          {sidebarOpen ? <X size={16} /> : <Menu size={16} />}
        </button>

        {/* Brand */}
        <div style={{ display: 'flex', alignItems: 'center', gap: 10, flexShrink: 0, paddingRight: 16 }}>
          {campaign.logoUrl ? (
            <img src={campaign.logoUrl} alt={campaign.shortName}
              style={{ width: 28, height: 28, objectFit: 'contain', borderRadius: 4, flexShrink: 0 }} />
          ) : (
            <div style={{
              width: 28, height: 28, borderRadius: 4, flexShrink: 0,
              background: 'linear-gradient(135deg, var(--accent) 0%, var(--accent-dim) 100%)',
              display: 'flex', alignItems: 'center', justifyContent: 'center',
              fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.7rem', color: '#000', letterSpacing: '0.04em',
            }}>
              {campaign.shortName.slice(0, 2)}
            </div>
          )}
          <div>
            <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.9rem', letterSpacing: '0.2em', color: 'var(--text)', lineHeight: 1 }}>
              {campaign.name}
            </div>
            <div style={{ marginTop: 2 }}>
              {isLive ? (
                <span className="vs-badge vs-badge-live">
                  <span style={{ width: 5, height: 5, borderRadius: '50%', background: '#4ade80', display: 'inline-block' }} className="vs-pulse" />
                  LIVE
                </span>
              ) : (
                <span className="vs-badge vs-badge-offline">OFFLINE</span>
              )}
            </div>
          </div>
        </div>

        {/* Secondary readouts -- collapse on mobile, there's no room and
            none of these are essential to have visible at all times */}
        <div className="topbar-secondary">
          <Sep />
          <div style={{ padding: '0 4px' }}><Clock /></div>

          {stats?.active_round && (
            <><Sep /><div style={{ padding: '0 4px' }}><SessionTimer start={stats.active_round.start} /></div></>
          )}
          {stats?.restart_at && (
            <><Sep /><div style={{ padding: '0 4px' }}><RestartCountdown restartAt={stats.restart_at} /></div></>
          )}
          {stats?.weather && (
            <><Sep /><WeatherPill w={stats.weather} /></>
          )}
          {total > 0 && (
            <><Sep /><TerritoryBar bluePct={bluePct} redPct={redPct} /></>
          )}
        </div>

        {/* Spacer */}
        <div style={{ flex: 1 }} />

        {/* Online pilots */}
        {stats && (
          <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
              <span style={{ width: 7, height: 7, borderRadius: '50%', background: campaign.blueColor, display: 'inline-block' }} />
              <span className="font-mono-vs" style={{ fontSize: '0.8rem', color: campaign.blueColor, fontWeight: 700 }}>{stats.blue_online}</span>
            </div>
            <div style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
              <span style={{ width: 7, height: 7, borderRadius: '50%', background: campaign.redColor, display: 'inline-block' }} />
              <span className="font-mono-vs" style={{ fontSize: '0.8rem', color: campaign.redColor, fontWeight: 700 }}>{stats.red_online}</span>
            </div>
          </div>
        )}

        {/* Round selector */}
        {rounds.length > 0 && (
          <>
            <Sep />
            <select
              value={selectedRound ?? ''}
              onChange={e => setSelectedRound(e.target.value === '' ? undefined : Number(e.target.value))}
              className="vs-input"
              style={{ fontSize: '0.68rem', padding: '4px 8px', height: 28, maxWidth: 180, cursor: 'pointer' }}
            >
              {activeRound  && <option value="">{roundLabel(activeRound)}</option>}
              {!activeRound && <option value="">Latest Round</option>}
              {pastRounds.map(r => <option key={r.id} value={r.id}>{roundLabel(r)}</option>)}
            </select>
          </>
        )}

        {/* Server IP */}
        {campaign.serverIp && (
          <div className="topbar-secondary">
            <Sep />
            <div style={{ display: 'flex', alignItems: 'center', gap: 5, flexShrink: 0 }}>
              <Server size={11} style={{ color: 'var(--text-dim)' }} />
              <span className="font-mono-vs" style={{ fontSize: '0.68rem', color: 'var(--text-dim)' }}>
                {campaign.serverIp}
              </span>
            </div>
          </div>
        )}

        <Sep />

        {/* User */}
        {user ? (
          <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
            {user.avatar ? (
              <img
                src={`https://cdn.discordapp.com/avatars/${user.discord_id}/${user.avatar}.webp?size=32`}
                alt=""
                style={{ width: 28, height: 28, borderRadius: '50%', flexShrink: 0, border: '2px solid var(--border-light)' }}
              />
            ) : (
              <div style={{ width: 28, height: 28, borderRadius: '50%', background: 'var(--accent)', display: 'flex', alignItems: 'center', justifyContent: 'center', fontSize: '0.7rem', fontWeight: 700, color: '#000', flexShrink: 0 }}>
                {user.username[0]?.toUpperCase()}
              </div>
            )}
            <div style={{ lineHeight: 1.2 }}>
              <div style={{ fontSize: '0.75rem', color: 'var(--text)', fontWeight: 600, maxWidth: 100, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                {user.username}
              </div>
              {user.is_admin && (
                <div style={{ display: 'flex', alignItems: 'center', gap: 3, marginTop: 1 }}>
                  <Shield size={9} style={{ color: '#fbbf24' }} />
                  <span style={{ fontSize: '0.58rem', color: '#fbbf24', letterSpacing: '0.1em' }}>ADMIN</span>
                </div>
              )}
            </div>
            <button onClick={logout} title="Logout"
              style={{ display: 'flex', alignItems: 'center', background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }}>
              <LogOut size={14} />
            </button>
          </div>
        ) : (
          <button
            onClick={() => navigate('/login')}
            style={{
              display: 'flex', alignItems: 'center', gap: 6, flexShrink: 0,
              fontSize: '0.72rem', fontWeight: 600, color: '#7289DA',
              background: 'rgba(114,137,218,0.08)',
              border: '1px solid rgba(114,137,218,0.25)', cursor: 'pointer',
              padding: '5px 12px', borderRadius: 5, letterSpacing: '0.06em', whiteSpace: 'nowrap',
            }}
          >
            <DiscordIcon size={11} /> Login
          </button>
        )}
      </header>

      {/* ══════════════════════════════════════════════════════════
          BODY
          ══════════════════════════════════════════════════════════ */}
      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>

        {/* Backdrop -- closes the drawer on mobile when tapped outside it */}
        <div
          className={`sidebar-backdrop${sidebarOpen ? ' open' : ''}`}
          onClick={() => setSidebarOpen(false)}
        />

        {/* SIDEBAR */}
        <aside className={`sidebar${sidebarOpen ? ' open' : ''}`}>

          {/* Nav section */}
          <div className="nav-group-label">Navigation</div>
          <nav style={{ flex: 1 }}>
            {allNav.map(({ to, icon: Icon, label }) => (
              <NavLink
                key={to}
                to={to}
                end={to === '/' || to === '/admin'}
                onClick={() => setSidebarOpen(false)}
                className={({ isActive }) => `nav-item${isActive ? ' active' : ''}`}
              >
                <Icon size={16} style={{ flexShrink: 0 }} />
                <span>{label}</span>
                {to === '/kills' && (
                  <span style={{ marginLeft: 'auto', opacity: 0.4 }}><ChevronRight size={12} /></span>
                )}
              </NavLink>
            ))}
          </nav>

          {/* Donation link */}
          {campaign.donationUrl && (
            <div style={{ padding: '8px 10px', borderTop: '1px solid var(--border)' }}>
              <a
                href={campaign.donationUrl}
                target="_blank"
                rel="noreferrer"
                className="nav-item"
                style={{ color: '#fb923c', borderRadius: 5, height: 36 }}
              >
                <Zap size={14} style={{ flexShrink: 0 }} />
                <span>SUPPORT</span>
              </a>
            </div>
          )}

          {/* Footer */}
          <div style={{ padding: '8px 16px', borderTop: '1px solid var(--border)', display: 'flex', alignItems: 'center', gap: 6 }}>
            <Radio size={10} style={{ color: 'var(--accent)', opacity: 0.5 }} />
            <span style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.08em' }}>FOWL ENGINE</span>
            <Plane size={9} style={{ color: 'var(--text-dim)', opacity: 0.4, marginLeft: 'auto' }} />
          </div>
        </aside>

        {/* MAIN */}
        <main style={{ flex: 1, overflow: 'hidden', display: 'flex', flexDirection: 'column', minWidth: 0, background: 'var(--bg)' }}>
          <Outlet />
        </main>

      </div>
    </div>
  )
}
