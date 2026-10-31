import React from 'react'
import { Link, Outlet, useNavigate, useLocation } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api, roundNo, type Round, type ServerInstance } from '../api'
import { useRound } from '../context/RoundContext'
import { useAuth } from '../context/AuthContext'
import { campaign } from '../config/campaign'
import { BRAND, syncSideColors } from '../config/redstorm'
import { ThemeToggle, useTheme } from '@shared/web/theme'
import { useInstance } from '../context/InstanceContext'
import Backdrop from './Backdrop'
import StatusBanner from './StatusBanner'
import { useOffline } from '@shared/web/snapshot'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import NotificationBell from './NotificationBell'
import { getDevUser } from '../lib/devAuth'
import { Emblem } from './Emblem'
import Flaps from './shell/Flaps'
import { functionKeys, isActive, isTyping, moreGroups, shortcutIndex } from './shell/keys'
import FunctionKeyStrip from './shell/FunctionKeys'
import { ChevronDown, ExternalLink, Lock, LogOut, Shield } from '@icons'

// ─────────────────────────────────────────────────────────────────────────────
// The shell: NATO Southern Command's situation-room console.
//
//   ┌ classification banner ───────────────────────────────────────────────┐
//   │ emblem · name · status lamp │ Zulu │ mission │ restart │ on station … │
//   │ [F1 SITUATION] [F2 PLOT] [F3 WIRE] … [F8 BRIEFING]          [MORE ▾] │
//   └──────────────────────────────────────────────────────────────────────┘
//
// The function keys are the navigation; Alt+1..Alt+8 press them (the real
// F-keys stay the browser's). Everything else is under MORE.
// ─────────────────────────────────────────────────────────────────────────────

function pad(n: number) { return String(n).padStart(2, '0') }
function hms(s: number) {
  return `${pad(Math.floor(s / 3600))}:${pad(Math.floor((s % 3600) / 60))}:${pad(Math.floor(s % 60))}`
}

/** A once-a-second clock, shared by the readouts. */
function useNow(): number {
  const [now, setNow] = React.useState(() => Date.now())
  React.useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 1000)
    return () => clearInterval(t)
  }, [])
  return now
}

function Readout({ label, children, className = '' }: { label: React.ReactNode; children: React.ReactNode; className?: string }) {
  return (
    <div className={`rsr-readout ${className}`}>
      {children}
      <span className="rsr-readout-label">{label}</span>
    </div>
  )
}

function MissionClocks({ start, restartAt }: { start?: string; restartAt?: string | null }) {
  const now = useNow()
  const z = new Date(now)
  const zulu = `${pad(z.getUTCHours())}:${pad(z.getUTCMinutes())}:${pad(z.getUTCSeconds())}`
  const elapsed = start ? Math.max(0, Math.floor((now - Date.parse(start)) / 1000)) : null
  const remaining = restartAt ? Math.max(0, Math.floor((Date.parse(restartAt) - now) / 1000)) : null
  const date = z.toUTCString().slice(5, 16).toUpperCase()
  return (
    <>
      <Readout label={<><b>ZULU</b> · {date}</>}>
        <Flaps text={zulu} label={`${zulu} Zulu`} />
      </Readout>
      {elapsed != null && (
        <Readout label="MISSION ELAPSED" className="rsr-hide-lg">
          <Flaps text={hms(elapsed)} className="rsr-flaps--small" label={`Mission elapsed ${hms(elapsed)}`} />
        </Readout>
      )}
      {remaining != null && (
        <Readout label="RESTART IN" className={`rsr-hide-xl${remaining < 1800 ? ' rsr-readout--urgent' : ''}`}>
          <Flaps text={hms(remaining)} className="rsr-flaps--small" label={`Restart in ${hms(remaining)}`} />
        </Readout>
      )}
    </>
  )
}

/* A round's `scenario` is the sortie name of the .miz the campaign was
   started under, frozen at that moment: a history label, noise for the live
   round when the server is already named. */
function roundLabel(r: Round, named = true) {
  const n = roundNo(r)
  if (r.active) return named ? `${r.scenario} · Round ${n} — Active` : `Round ${n} — Active`
  const start = new Date(r.start).toLocaleDateString([], { month: 'short', day: 'numeric' })
  const end   = r.end ? new Date(r.end).toLocaleDateString([], { month: 'short', day: 'numeric' }) : '?'
  return `Round ${n} · ${r.scenario} · ${start}–${end}`
}

/* Round (and, unlocked with several servers, server) selector. Shown in the
   console on a desk and inside MORE on a phone; CSS shows one. */
function CampaignSelect({
  value, onChange, multi, currentId, activeRound, pastRounds, groups, style,
}: {
  value: string
  onChange: (value: string) => void
  multi: boolean
  currentId: string
  activeRound?: Round
  pastRounds: Round[]
  groups: { instance: ServerInstance; rounds: Round[] }[]
  style?: React.CSSProperties
}) {
  return (
    <select
      value={value}
      onChange={e => onChange(e.target.value)}
      className="vs-input"
      title={multi ? 'Server and campaign' : 'Round'}
      aria-label={multi ? 'Server and campaign' : 'Round'}
      style={{ fontSize: '0.68rem', padding: '4px 8px', height: 28, cursor: 'pointer', ...style }}
    >
      {!multi && <>
        {activeRound  && <option value={`${currentId}|`}>{roundLabel(activeRound)}</option>}
        {!activeRound && <option value={`${currentId}|`}>Latest Round</option>}
        {pastRounds.map(r => <option key={r.id} value={`${currentId}|${r.id}`}>{roundLabel(r)}</option>)}
      </>}
      {multi && groups.map(({ instance, rounds: rs }) => {
        const live = rs.find(r => r.active)
        const past = rs.filter(r => !r.active)
        return (
          <optgroup key={instance.id} label={`${instance.label}${instance.public === false ? ' [test]' : ''}`}>
            <option value={`${instance.id}|`}>{live ? roundLabel(live, false) : 'Latest Round'}</option>
            {past.map(r => (
              <option key={r.id} value={`${instance.id}|${r.id}`}>{roundLabel(r)}</option>
            ))}
          </optgroup>
        )
      })}
    </select>
  )
}

function MoreMenu({ controls }: { controls: React.ReactNode }) {
  const { user } = useAuth()
  const location = useLocation()
  const [open, setOpen] = React.useState(false)
  const box = React.useRef<HTMLDivElement>(null)
  const groups = moreGroups(user, { wikiUrl: campaign.wikiUrl, donationUrl: campaign.donationUrl })
  const anyActive = groups.some(g => g.items.some(i => !i.external && !i.locked && isActive(i.to, location.pathname, location.search)))

  React.useEffect(() => {
    if (!open) return
    const onDown = (e: MouseEvent) => { if (!box.current?.contains(e.target as Node)) setOpen(false) }
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') setOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => { document.removeEventListener('mousedown', onDown); document.removeEventListener('keydown', onKey) }
  }, [open])

  return (
    <div className="rsr-more" ref={box}>
      <button
        type="button"
        className={`rsr-fkey rsr-fkey-more${anyActive ? ' active' : ''}`}
        aria-expanded={open}
        aria-haspopup="true"
        aria-controls="rsr-more-menu"
        onClick={() => setOpen(v => !v)}
      >
        <span className="rsr-fkey-code" aria-hidden="true">··</span>
        <span>More</span>
        <ChevronDown size={12} />
      </button>
      {open && (
        <div className="rsr-more-menu" id="rsr-more-menu">
          <div className="rsr-more-controls">{controls}</div>
          {groups.map(g => (
            <div key={g.label} className="rsr-more-group">
              <div className="rsr-more-label">{g.label}</div>
              {g.items.map(i => i.external ? (
                <a key={i.label} href={i.to} target="_blank" rel="noreferrer" className="rsr-more-item" onClick={() => setOpen(false)}>
                  {i.label}<span className="rsr-more-end"><ExternalLink size={11} /></span>
                </a>
              ) : (
                <Link
                  key={i.label}
                  to={i.locked ? i.locked.go : i.to}
                  className={['rsr-more-item', i.locked ? 'locked' : '', !i.locked && isActive(i.to, location.pathname, location.search) ? 'active' : ''].filter(Boolean).join(' ')}
                  title={i.locked?.reason}
                  onClick={() => setOpen(false)}
                >
                  {i.label}
                  {i.locked && <span className="rsr-more-end" aria-label="locked"><Lock size={11} /></span>}
                </Link>
              ))}
            </div>
          ))}
          <p className="rsr-more-keys">Keys: Alt+1 to Alt+8 press F1 to F8.</p>
        </div>
      )}
    </div>
  )
}

function DiscordIcon({ size = 11 }: { size?: number }) {
  return (
    <svg width={size} height={size} viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
      <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
    </svg>
  )
}

export default function Layout() {
  const { user, logout } = useAuth()
  const navigate = useNavigate()
  const location = useLocation()
  const { theme } = useTheme()
  // Script-drawn side colours follow the theme; set before the page renders.
  syncSideColors(theme)

  const { data: stats } = useQuery({ queryKey: ['stats'], queryFn: api.stats, refetchInterval: 30_000 })
  const { instances, selected: selectedInstance, current, multi, select, locked } = useInstance()
  const { data: rounds = [] } = useQuery({
    queryKey: ['rounds', multi],
    queryFn: () => api.rounds(multi),
    refetchInterval: 60_000,
  })
  const { selectedRound, setSelectedRound } = useRound()
  const isDevUser = !!getDevUser()

  // Three states, not two: a round row in sled survives the DCS server
  // crashing, which is exactly when a plain LIVE lamp would lie.
  const { data: health } = useQuery({ queryKey: ['health'], queryFn: api.health, refetchInterval: 30_000, retry: false })
  const { offline: bfdbDown } = useOffline()
  const liveState: 'live' | 'engine-down' | 'offline' =
    bfdbDown ? 'offline'
      : health?.engine.reachable ? 'live'
      : health ? (health.active_round ? 'engine-down' : 'offline')
        : (stats?.active_round ? 'live' : 'offline')

  const instanceOf  = (r: { instance?: string }) => r.instance ?? current?.id
  const ownRounds   = multi ? rounds.filter(r => instanceOf(r) === current?.id) : rounds
  const activeRound = ownRounds.find(r => r.active)
  const pastRounds  = ownRounds.filter(r => !r.active)
  const selectorValue = `${selectedInstance ?? current?.id ?? ''}|${selectedRound ?? ''}`
  function onSelectCampaign(value: string) {
    const [inst, round] = value.split('|')
    if (!locked && inst && inst !== current?.id) select(inst)
    setSelectedRound(round === '' ? undefined : Number(round))
  }
  const groups = (multi ? instances : []).map(i => ({ instance: i, rounds: rounds.filter(r => instanceOf(r) === i.id) }))
  const hasCampaignSelect = rounds.length > 0 || multi
  const campaignSelect = (style?: React.CSSProperties) => hasCampaignSelect && (
    <CampaignSelect
      value={selectorValue}
      onChange={onSelectCampaign}
      multi={multi}
      currentId={current?.id ?? ''}
      activeRound={activeRound}
      pastRounds={pastRounds}
      groups={groups}
      style={style}
    />
  )

  const keys = React.useMemo(() => functionKeys(user), [user])

  // Alt+1..Alt+8 press F1..F8. The real F-keys stay the browser's.
  React.useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      const i = shortcutIndex(e, isTyping(e.target))
      if (i == null) return
      const k = keys[i]
      if (!k) return
      e.preventDefault()
      navigate(k.locked ? k.locked.go : k.to)
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [keys, navigate])

  const lamp = liveState === 'live'
    ? { cls: 'rsr-lamp--live', text: 'NET LIVE', title: 'The campaign server is running' }
    : liveState === 'engine-down'
      ? { cls: 'rsr-lamp--degraded', text: 'NO ENGINE', title: health?.engine.error ?? 'The round is open but the DCS server is not answering.' }
      : { cls: 'rsr-lamp--offline', text: 'OFF NET', title: 'The campaign server is offline' }

  return (
    <>
      <Backdrop />
      <div className="vs-shell rsr-shell">
        <div className="rsr-classbar" role="note" aria-label="Classification banner (fictional)">
          <span className="rsr-classbar-side">NATO SOUTHCOM</span>
          <span>{BRAND.banner}</span>
          <span className="rsr-classbar-side">SITUATION ROOM</span>
        </div>

        <header className="rsr-console">
          <div className="rsr-console-row">
            <Link to="/" className="rsr-brand" aria-label={`${BRAND.name}: situation`}>
              <Emblem size={38} onDark />
              <div>
                <div className="rsr-brand-name">
                  <span className="rsr-brand-long">{BRAND.name}</span>
                  <span className="rsr-brand-short">SF·85</span>
                </div>
                <div className="rsr-brand-sub">NATO Southern Command</div>
              </div>
            </Link>

            <span className={`rsr-lamp ${lamp.cls}`} title={lamp.title}>
              <span className="rsr-lamp-bulb" aria-hidden="true" />
              <span className="rsr-hide-sm">{lamp.text}</span>
            </span>

            <div className="rsr-console-sep rsr-hide-sm" />
            <div className="rsr-hide-sm" style={{ display: 'flex', gap: 14, alignItems: 'center' }}>
              <MissionClocks start={stats?.active_round?.start} restartAt={stats?.restart_at} />
            </div>

            {stats && (
              <>
                <div className="rsr-console-sep rsr-hide-md" />
                <Readout label="ON STATION" className="rsr-hide-md">
                  <span style={{ display: 'inline-flex', gap: 6, alignItems: 'center' }}>
                    <Flaps text={pad(stats.blue_online)} className="rsr-flaps--small rsr-flaps--blue" label={`${campaign.blueLabel} ${stats.blue_online} online`} />
                    <Flaps text={pad(stats.red_online)} className="rsr-flaps--small rsr-flaps--red" label={`${campaign.redLabel} ${stats.red_online} online`} />
                  </span>
                </Readout>
              </>
            )}

            <div className="rsr-console-tools">
              <div className="rsr-hide-md">{campaignSelect({ maxWidth: multi ? 260 : 200 })}</div>
              <NotificationBell />
              <div className="rsr-hide-md"><ThemeToggle className="vs-touch-target" style={{ background: '#0d0f11', borderColor: '#3a4148', color: '#aab2b8' }} /></div>
              {user ? (
                <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexShrink: 0 }}>
                  {user.avatar ? (
                    <img
                      src={`https://cdn.discordapp.com/avatars/${user.discord_id}/${user.avatar}.webp?size=32`}
                      alt=""
                      style={{ width: 28, height: 28, borderRadius: 2, flexShrink: 0, border: '1px solid #3a4148', filter: 'grayscale(0.4) contrast(1.1)' }}
                    />
                  ) : (
                    <div style={{ width: 28, height: 28, borderRadius: 2, background: '#ffb000', display: 'flex', alignItems: 'center', justifyContent: 'center', fontSize: '0.75rem', fontWeight: 700, color: '#14110a', flexShrink: 0 }}>
                      {user.username[0]?.toUpperCase()}
                    </div>
                  )}
                  <div className="rsr-hide-lg" style={{ lineHeight: 1.2 }}>
                    <div style={{ fontSize: '0.72rem', fontWeight: 700, maxWidth: 110, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                      {user.username}
                    </div>
                    {user.is_admin && (
                      <div style={{ display: 'flex', alignItems: 'center', gap: 3 }}>
                        <Shield size={9} style={{ color: '#e8c34a' }} />
                        <span style={{ fontSize: '0.56rem', color: '#e8c34a', letterSpacing: '0.14em' }}>ADMIN</span>
                      </div>
                    )}
                    {import.meta.env.DEV && isDevUser && (
                      <div style={{ fontSize: '0.55rem', color: '#e8c34a', letterSpacing: '0.12em' }}>DEV SESSION</div>
                    )}
                  </div>
                  <button onClick={logout} title="Log out" aria-label="Log out" className="vs-touch-target"
                    style={{ display: 'flex', alignItems: 'center', background: 'none', border: 'none', cursor: 'pointer', color: '#8c959c', padding: 0 }}>
                    <LogOut size={14} />
                  </button>
                </div>
              ) : (
                <button type="button" onClick={() => navigate('/login')} className="rsr-login">
                  <DiscordIcon /> <span className="rsr-hide-sm">Sign in</span>
                </button>
              )}
            </div>
          </div>

          <nav className="rsr-fkeys" aria-label="Function keys">
            <FunctionKeyStrip keys={keys} pathname={location.pathname} search={location.search} />
            <MoreMenu
              controls={<>
                {campaignSelect({ flex: 1, minWidth: 0, height: 34 })}
                <ThemeToggle className="vs-touch-target" style={{ width: 34, height: 34, background: '#0d0f11', borderColor: '#3a4148', color: '#aab2b8' }} />
              </>}
            />
          </nav>
        </header>

        <main className="vs-main rsr-main">
          <StatusBanner />
          {/* A crashed page keeps the console; moving to another page resets
              it. The theme is in the key so a page re-reads its side colours. */}
          <ErrorBoundary key={`${location.pathname}|${theme}`}>
            <Outlet />
          </ErrorBoundary>
        </main>
      </div>
    </>
  )
}
