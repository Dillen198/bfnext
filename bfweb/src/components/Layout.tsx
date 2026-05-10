import React from 'react'
import { NavLink, Outlet, useNavigate } from 'react-router-dom'
import {
  LayoutDashboard, Map, Target, BarChart3, Users, Crosshair,
  WifiOff, Zap, LogOut, Shield, Settings, Info, Server,
} from 'lucide-react'
import { useQuery } from '@tanstack/react-query'
import { api, type Weather } from '../api'
import { useRound } from '../context/RoundContext'
import { useAuth } from '../context/AuthContext'
import { campaign } from '../config/campaign'

// ── Nav items ─────────────────────────────────────────────────────────────────

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

// ── Helpers ───────────────────────────────────────────────────────────────────

function formatDuration(seconds: number): string {
  const h = Math.floor(seconds / 3600)
  const m = Math.floor((seconds % 3600) / 60)
  const s = Math.floor(seconds % 60)
  return `${String(h).padStart(2, '0')}:${String(m).padStart(2, '0')}:${String(s).padStart(2, '0')}`
}

// ── Top-bar sub-components ────────────────────────────────────────────────────

function Clock() {
  const [now, setNow] = React.useState(new Date())
  React.useEffect(() => {
    const t = setInterval(() => setNow(new Date()), 1000)
    return () => clearInterval(t)
  }, [])
  const d = now.toUTCString()
  return (
    <div style={{ lineHeight: 1, textAlign: 'center' }}>
      <div className="font-mono-vs tabular-nums" style={{ fontSize: '0.95rem', fontWeight: 700, color: 'var(--text)', letterSpacing: '0.06em' }}>
        {d.slice(17, 25)}
      </div>
      <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textTransform: 'uppercase', marginTop: 2 }}>
        {d.slice(0, 16)} UTC
      </div>
    </div>
  )
}

function SessionTimer({ start }: { start: string }) {
  const [elapsed, setElapsed] = React.useState(0)
  React.useEffect(() => {
    const startMs = new Date(start).getTime()
    const tick = () => setElapsed(Math.max(0, Math.floor((Date.now() - startMs) / 1000)))
    tick()
    const t = setInterval(tick, 1000)
    return () => clearInterval(t)
  }, [start])
  return (
    <div style={{ lineHeight: 1, textAlign: 'center' }}>
      <div className="font-mono-vs tabular-nums" style={{ fontSize: '0.95rem', fontWeight: 700, color: 'var(--text-muted)', letterSpacing: '0.06em' }}>
        {formatDuration(elapsed)}
      </div>
      <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textTransform: 'uppercase', marginTop: 2 }}>
        Elapsed
      </div>
    </div>
  )
}

function RestartCountdown({ restartAt }: { restartAt: string }) {
  const [remaining, setRemaining] = React.useState(0)
  React.useEffect(() => {
    const endMs = new Date(restartAt).getTime()
    const tick = () => setRemaining(Math.max(0, Math.floor((endMs - Date.now()) / 1000)))
    tick()
    const t = setInterval(tick, 1000)
    return () => clearInterval(t)
  }, [restartAt])
  const urgent = remaining < 1800
  return (
    <div style={{ lineHeight: 1, textAlign: 'center' }}>
      <div className="font-mono-vs tabular-nums" style={{ fontSize: '0.85rem', fontWeight: 700, color: urgent ? 'var(--accent)' : 'var(--text-muted)', letterSpacing: '0.06em' }}>
        {formatDuration(remaining)}
      </div>
      <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textTransform: 'uppercase', marginTop: 2 }}>
        Restart
      </div>
    </div>
  )
}

// ── Weather pill (condensed for top bar) ──────────────────────────────────────

const COMPASS = ['N','NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W','WNW','NW','NNW']
function windDirName(deg: number) { return COMPASS[Math.round(deg / 22.5) % 16] }

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
  VFR: '#22c55e', MVFR: '#3b82f6', IFR: '#ef4444', LIFR: '#a855f7',
}

const CLOUD_LABEL: Record<number, string> = {
  0:'SKC', 1:'FEW', 2:'FEW', 3:'FEW', 4:'FEW', 5:'SCT', 6:'SCT', 7:'BKN', 8:'OVC',
}
function cloudCover(density: number | null) {
  if (density === null) return 'CLR'
  return CLOUD_LABEL[Math.round(Math.max(0, Math.min(8, density)))] ?? 'CLR'
}

function WeatherPill({ weather }: { weather: Weather }) {
  const density    = weather.cloud_density ?? null
  const hasCeiling = density !== null && density >= 5
  const cloudFt    = Math.round(weather.cloud_base_m * 3.281 / 100) * 100
  const ceilingFt  = hasCeiling ? cloudFt : null
  const rule       = flightRule(ceilingFt, weather.visibility_m ?? null)
  const color      = RULE_COLOR[rule]
  const calm       = weather.wind_speed_kts < 1
  const visMStr    = weather.visibility_m !== null
    ? weather.visibility_m >= 9999 ? '10+km' : `${(weather.visibility_m / 1000).toFixed(1)}km`
    : null
  const coverLabel = cloudCover(density)
  const ceilStr    = hasCeiling
    ? cloudFt >= 10000 ? `${Math.round(cloudFt / 1000)}k` : `${cloudFt}`
    : null

  const stat = (label: string, val: string, valColor = 'var(--text-muted)') => (
    <div style={{ textAlign: 'center', lineHeight: 1 }}>
      <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textTransform: 'uppercase', marginBottom: 2 }}>{label}</div>
      <div className="font-mono-vs" style={{ fontSize: '0.7rem', color: valColor, fontWeight: 600 }}>{val}</div>
    </div>
  )

  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
      {/* Flight rule badge */}
      <div style={{ textAlign: 'center', lineHeight: 1 }}>
        <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.12em', marginBottom: 3 }}>COND</div>
        <span style={{
          fontSize: '0.72rem', fontWeight: 700, letterSpacing: '0.1em',
          color, border: `1px solid ${color}55`, padding: '2px 7px', borderRadius: 2,
          background: `${color}0d`, lineHeight: 1.5, display: 'inline-block',
        }}>
          {rule}
        </span>
      </div>

      <div style={{ width: 1, height: 28, background: 'var(--border)', flexShrink: 0 }} />

      {/* Wind */}
      {stat('WIND', calm ? 'CALM' : `${windDirName(weather.wind_from_deg)} ${Math.round(weather.wind_speed_kts)}kt`)}

      {/* Temp */}
      {stat('TEMP', `${Math.round(weather.temp_c)}°C`)}

      {/* QNH */}
      {stat('QNH', `${Math.round(weather.qnh_hpa)}`)}

      {/* Clouds / ceiling */}
      {stat('SKY', ceilStr ? `${coverLabel} ${ceilStr}ft` : coverLabel)}

      {/* Visibility */}
      {visMStr && stat('VIS', visMStr)}
    </div>
  )
}

// ── Territory mini-bar ────────────────────────────────────────────────────────

function TerritoryPill({
  bluePct, redPct,
}: {
  bluePct: number; redPct: number;
}) {
  const neutPct = Math.max(0, 100 - bluePct - redPct)
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 6, flexShrink: 0 }}>
      <div style={{ lineHeight: 1 }}>
        <div style={{ width: 84, height: 6, overflow: 'hidden', display: 'flex', background: 'rgba(75,85,99,0.12)', borderRadius: 1 }}>
          <div style={{ width: `${bluePct}%`, height: '100%', background: 'var(--blue)', transition: 'width 0.5s' }} />
          <div style={{ width: `${neutPct}%`, height: '100%', background: '#374151',   transition: 'width 0.5s' }} />
          <div style={{ width: `${redPct}%`,  height: '100%', background: 'var(--red)', transition: 'width 0.5s' }} />
        </div>
        <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: 2 }}>
          <span className="font-mono-vs" style={{ fontSize: '0.68rem', color: 'var(--blue)' }}>{bluePct}%</span>
          <span className="font-mono-vs" style={{ fontSize: '0.68rem', color: 'var(--red)' }}>{redPct}%</span>
        </div>
      </div>
    </div>
  )
}

// ── Icon nav ──────────────────────────────────────────────────────────────────

function HeartIcon() {
  return (
    <svg width="13" height="13" viewBox="0 0 24 24" fill="currentColor">
      <path d="M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z"/>
    </svg>
  )
}

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

  const isLive      = !!stats?.active_round
  const activeRound = rounds.find(r => r.active)
  const pastRounds  = rounds.filter(r => !r.active)

  function roundLabel(r: { id: number; scenario: string; start: string; end: string | null; active: boolean }) {
    if (r.active) return `${r.scenario} (Active)`
    const start = new Date(r.start).toLocaleDateString()
    const end   = r.end ? new Date(r.end).toLocaleDateString() : '?'
    return `${r.scenario} · ${start}–${end}`
  }

  const allNav = [...NAV, ...(user?.is_admin ? [ADMIN_NAV] : [])]

  const Sep = () => <div className="topbar-sep" />

  return (
    <div style={{ display: 'flex', flexDirection: 'column', height: '100vh', overflow: 'hidden', background: 'var(--bg)' }}>

      {/* ── TOP BAR ────────────────────────────────────────────────────────── */}
      <header style={{
        height: 52,
        flexShrink: 0,
        display: 'flex',
        alignItems: 'center',
        gap: 12,
        padding: '0 14px',
        background: '#050505',
        borderBottom: '1px solid var(--border)',
      }}>

        {/* Identity */}
        <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
          {campaign.logoUrl ? (
            <img
              src={campaign.logoUrl}
              alt={campaign.shortName}
              style={{ width: 30, height: 30, objectFit: 'contain', borderRadius: 2, flexShrink: 0 }}
            />
          ) : (
            <div style={{
              width: 30, height: 30, background: 'var(--accent)', borderRadius: 2, flexShrink: 0,
              display: 'flex', alignItems: 'center', justifyContent: 'center',
              fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.78rem', letterSpacing: '0.04em', color: '#fff',
            }}>
              {campaign.shortName}
            </div>
          )}
          <span style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1rem', letterSpacing: '0.2em', color: 'var(--text)', whiteSpace: 'nowrap' }}>
            {campaign.name}
          </span>
          {isLive ? (
            <span className="vs-badge vs-badge-live">
              <span style={{ width: 5, height: 5, borderRadius: '50%', background: '#4ade80', display: 'inline-block' }} className="vs-pulse" />
              LIVE
            </span>
          ) : (
            <span className="vs-badge vs-badge-offline">
              <WifiOff size={8} />
              OFFLINE
            </span>
          )}
        </div>

        <Sep />
        <Clock />

        {stats?.active_round && (
          <>
            <Sep />
            <SessionTimer start={stats.active_round.start} />
          </>
        )}

        {stats?.restart_at && (
          <>
            <Sep />
            <RestartCountdown restartAt={stats.restart_at} />
          </>
        )}

        {stats?.weather && (
          <>
            <Sep />
            <WeatherPill weather={stats.weather} />
          </>
        )}

        {total > 0 && (
          <>
            <Sep />
            <TerritoryPill bluePct={bluePct} redPct={redPct} />
          </>
        )}

        {/* Push right */}
        <div style={{ flex: 1 }} />

        {/* Online count */}
        {stats && (
          <div style={{ display: 'flex', alignItems: 'center', gap: 6, flexShrink: 0 }}>
            <span className="font-mono-vs" style={{ fontSize: '0.8rem', color: campaign.blueColor, fontWeight: 700 }}>
              {campaign.blueLabel.slice(0, 1)}:{stats.blue_online}
            </span>
            <span style={{ fontSize: '0.68rem', color: 'var(--text-dim)' }}>·</span>
            <span className="font-mono-vs" style={{ fontSize: '0.8rem', color: campaign.redColor, fontWeight: 700 }}>
              {campaign.redLabel.slice(0, 1)}:{stats.red_online}
            </span>
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
              style={{ fontSize: '0.6rem', padding: '2px 5px', height: 26, maxWidth: 160, cursor: 'pointer' }}
            >
              {activeRound && <option value="">{roundLabel(activeRound)}</option>}
              {!activeRound && <option value="">Latest Round</option>}
              {pastRounds.map(r => (
                <option key={r.id} value={r.id}>{roundLabel(r)}</option>
              ))}
            </select>
          </>
        )}

        {/* Server IP */}
        {campaign.serverIp && (
          <>
            <Sep />
            <div style={{ display: 'flex', alignItems: 'center', gap: 5, flexShrink: 0 }}>
              <Server size={12} style={{ color: 'var(--accent)', flexShrink: 0 }} />
              <span className="font-mono-vs" style={{ fontSize: '0.72rem', color: '#64748b', letterSpacing: '0.04em' }}>
                {campaign.serverIp}
              </span>
            </div>
          </>
        )}

        <Sep />

        {/* User */}
        {user ? (
          <div style={{ display: 'flex', alignItems: 'center', gap: 6, flexShrink: 0 }}>
            {user.avatar ? (
              <img
                src={`https://cdn.discordapp.com/avatars/${user.discord_id}/${user.avatar}.webp?size=32`}
                alt=""
                style={{ width: 26, height: 26, borderRadius: '50%', flexShrink: 0 }}
              />
            ) : (
              <div style={{ width: 26, height: 26, borderRadius: '50%', background: 'var(--accent)', flexShrink: 0 }} />
            )}
            <div style={{ lineHeight: 1 }}>
              <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontWeight: 600, maxWidth: 96, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                {user.username}
              </div>
              {user.is_admin && (
                <div style={{ display: 'flex', alignItems: 'center', gap: 3, marginTop: 1 }}>
                  <Shield size={10} style={{ color: '#f59e0b' }} />
                  <span style={{ fontSize: '0.65rem', color: '#f59e0b', letterSpacing: '0.1em' }}>ADMIN</span>
                </div>
              )}
            </div>
            <button
              onClick={logout}
              title="Logout"
              style={{ display: 'flex', alignItems: 'center', background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-dim)', padding: 0 }}
            >
              <LogOut size={14} />
            </button>
          </div>
        ) : (
          <button
            onClick={() => navigate('/login')}
            style={{
              display: 'flex', alignItems: 'center', gap: 5, flexShrink: 0,
              fontSize: '0.7rem', color: '#5865F2', background: 'none',
              border: '1px solid rgba(88,101,242,0.3)', cursor: 'pointer', padding: '4px 10px',
              borderRadius: 2, letterSpacing: '0.1em',
              fontFamily: "'Bebas Neue', sans-serif", whiteSpace: 'nowrap',
            }}
          >
            <DiscordIcon size={10} />
            LOGIN
          </button>
        )}

      </header>

      {/* ── BODY ────────────────────────────────────────────────────────────── */}
      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>

        {/* ICON SIDEBAR */}
        <aside style={{
          width: 60,
          flexShrink: 0,
          display: 'flex',
          flexDirection: 'column',
          background: '#060606',
          borderRight: '1px solid var(--border)',
          zIndex: 50,
        }}>
          <nav style={{ flex: 1, paddingTop: 4 }}>
            {allNav.map(({ to, icon: Icon, label }) => (
              <NavLink
                key={to}
                to={to}
                end={to === '/'}
                className={({ isActive }) => `nav-icon-btn${isActive ? ' active' : ''}`}
              >
                <Icon size={20} />
                <span className="nav-tooltip">{label}</span>
              </NavLink>
            ))}
          </nav>

          {campaign.donationUrl && (
            <div style={{ padding: '6px 4px', borderTop: '1px solid var(--border)' }}>
              <a
                href={campaign.donationUrl}
                target="_blank"
                rel="noreferrer"
                className="nav-icon-btn"
                style={{ color: '#f97316', height: 44 }}
              >
                <HeartIcon />
                <span className="nav-tooltip">SUPPORT</span>
              </a>
            </div>
          )}

          <div style={{ padding: '5px 0', borderTop: '1px solid var(--border)', display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
            <Zap size={11} style={{ color: 'var(--accent)', opacity: 0.3 }} />
          </div>
        </aside>

        {/* MAIN CONTENT */}
        <main style={{ flex: 1, overflow: 'hidden', display: 'flex', flexDirection: 'column', minWidth: 0, background: 'var(--bg)' }}>
          <Outlet />
        </main>

      </div>
    </div>
  )
}
