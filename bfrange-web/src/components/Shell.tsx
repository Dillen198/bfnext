/**
 * The frame every page sits in. On a desk: a rail on the left with the
 * pages grouped by what you are doing (flying, debriefing, competing), a top
 * bar that always shows whether the range is up and what it is doing, and
 * search on Ctrl/⌘ K. On a phone: the same top bar, a tab bar along the
 * bottom for the five pages you use from the cockpit, and the rest in a
 * drawer.
 */
import { useEffect, useState, type ReactNode } from 'react'
import { Link, NavLink, useLocation } from 'react-router-dom'
import { ChevronsLeft, ChevronsRight, LogOut, Menu, Moon, Search, Sun, X } from '@icons'
import { MOCK } from '../api'
import { useAuth } from '../context/AuthContext'
import { useTheme } from '../context/ThemeContext'
import { fmt, pad3 } from '../lib/format'
import { useLive } from '../lib/useLive'
import { useMyResultAlerts } from '../lib/useMyResultAlerts'
import { usePref } from '../lib/usePref'
import { CommandPalette } from './CommandPalette'
import { NAV, NAV_GROUPS, navFor } from './nav'
import { RangeLockup, RangeMark } from './RangeMark'

function DiscordMark() {
  return (
    <svg width="14" height="14" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
      <path d="M20.3 4.4A19.8 19.8 0 0 0 15.4 3l-.6 1.3a18.3 18.3 0 0 0-5.6 0L8.6 3a19.7 19.7 0 0 0-4.9 1.5C.6 9.1-.3 13.7.1 18.2a19.9 19.9 0 0 0 6 3l1.3-2a12.9 12.9 0 0 1-2-1l.5-.4a14.2 14.2 0 0 0 12.2 0l.5.4-2 1 1.3 2a19.8 19.8 0 0 0 6-3c.5-5.2-.8-9.7-3.6-13.8zM8 15.4c-1.2 0-2.2-1.1-2.2-2.4s1-2.4 2.2-2.4 2.2 1.1 2.2 2.4-1 2.4-2.2 2.4zm8 0c-1.2 0-2.2-1.1-2.2-2.4s1-2.4 2.2-2.4 2.2 1.1 2.2 2.4-1 2.4-2.2 2.4z" />
    </svg>
  )
}

const isBool = (v: unknown): v is boolean => typeof v === 'boolean'
const isMac = () => {
  try { return /Mac|iPhone|iPad/.test(navigator.platform) } catch { return false }
}

/** LIVE / OFFLINE, the mission clock and who is up. Always in view. */
function RangeStatus() {
  const q = useLive()
  const live = q.data?.live
  if (q.isLoading) return <span className="status-pill"><span className="dot" style={{ background: 'var(--dim)' }} /> Connecting…</span>
  if (!live) {
    return (
      <span className="status-pill off" title={q.data?.reason ?? (q.error instanceof Error ? q.error.message : 'The range server is not publishing a live picture')}>
        <span className="dot" style={{ background: 'var(--wave)' }} /> Range offline
      </span>
    )
  }
  const up = live.players.filter(p => p.in_air).length
  const open = live.carriers.some(c => c.recovery_open)
  return (
    <Link to="/" className="status-pill" title={`${live.theatre} · ${live.mission_date} ${live.mission_time}${live.night ? ' · night' : ''}`}>
      <span className="dot live" />
      <b>LIVE</b>
      <span className="sep hidden sm:inline" />
      <span className="mono hidden sm:inline">{live.mission_time}{live.night ? ' N' : ''}</span>
      <span className="sep hidden md:inline" />
      <span className="mono hidden md:inline">{up}/{live.players.length} up</span>
      <span className="sep hidden xl:inline" />
      <span className="mono hidden xl:inline">{pad3(live.wind.surface_from_deg)}/{fmt(live.wind.surface_kts)}</span>
      {open && <><span className="sep hidden xl:inline" /><span className="hidden xl:inline" style={{ color: 'var(--datum)' }}>DECK OPEN</span></>}
    </Link>
  )
}

function UserButton({ compact = false }: { compact?: boolean }) {
  const { me, login, logout } = useAuth()
  if (!me?.logged_in) {
    return (
      <button className="btn-range sm" onClick={login}>
        <DiscordMark /> {compact ? 'Log in' : 'Log in'}
      </button>
    )
  }
  return (
    <div className="flex items-center gap-1">
      <Link
        to="/me"
        className="hidden sm:flex items-center gap-2 h-8 pl-2 pr-3 rounded-[3px] border border-[var(--line-2)] hover:border-[var(--haze)]"
        title={me.ucid ? 'Your range record' : 'Your Discord is not linked to a DCS account yet'}
      >
        <span className={`dot ${me.ucid ? 'live' : ''}`} style={me.ucid ? undefined : { background: 'var(--ball)' }} />
        <span className="text-[13px] font-medium max-w-[140px] truncate">{me.name ?? 'Pilot'}</span>
        {me.admin && <span className="chip warn" style={{ height: 16, fontSize: 9.5 }}>IP</span>}
      </Link>
      <button className="btn-range ghost sm" onClick={logout} title="Log out" aria-label="Log out">
        <LogOut size={15} />
      </button>
    </div>
  )
}

function RailLinks({ collapsed, onPick }: { collapsed: boolean; onPick?: () => void }) {
  return (
    <nav className="flex flex-col gap-4" aria-label="Main">
      {NAV_GROUPS.map(g => (
        <div key={g.label}>
          {!collapsed && <div className="rail-group">{g.label}</div>}
          <div className="flex flex-col gap-0.5">
            {g.items.map(n => (
              <NavLink key={n.to} to={n.to} end={n.end} onClick={onPick} className="rail-link" title={collapsed ? n.label : undefined}>
                <n.icon size={17} />
                {!collapsed && <span>{n.label}</span>}
              </NavLink>
            ))}
          </div>
        </div>
      ))}
    </nav>
  )
}

export function Shell({ children, bleed }: { children: ReactNode; bleed: boolean }) {
  const { theme, toggle } = useTheme()
  const [collapsed, setCollapsed] = usePref<boolean>('range.rail.collapsed', false, isBool)
  const [drawer, setDrawer] = useState(false)
  const [palette, setPalette] = useState(false)
  const loc = useLocation()
  const page = navFor(loc.pathname)
  useMyResultAlerts()

  // close the drawer when the page changes
  const [lastPath, setLastPath] = useState(loc.pathname)
  if (lastPath !== loc.pathname) {
    setLastPath(loc.pathname)
    setDrawer(false)
  }

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if ((e.key === 'k' || e.key === 'K') && (e.metaKey || e.ctrlKey)) {
        e.preventDefault()
        setPalette(p => !p)
      } else if (e.key === '/' && !palette && !(e.target instanceof HTMLInputElement || e.target instanceof HTMLTextAreaElement || e.target instanceof HTMLSelectElement)) {
        e.preventDefault()
        setPalette(true)
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [palette])

  return (
    <div className={`shell${collapsed ? ' collapsed' : ''}`}>
      <aside className="rail hidden lg:flex">
        <Link to="/" className="rail-brand" aria-label="Vector Strike Range, range control">
          {collapsed ? <RangeMark size={34} /> : <RangeLockup size={36} />}
        </Link>
        <div className="flex-1 overflow-y-auto px-2 py-3">
          <RailLinks collapsed={collapsed} />
        </div>
        <div className="rail-foot">
          {!collapsed && (
            <p className="m-0 text-[11.5px] dim leading-snug">
              Scores are graded by the range engine; the site explains them.
            </p>
          )}
          <button className="btn-range ghost sm self-start" onClick={() => setCollapsed(c => !c)} title={collapsed ? 'Expand the menu' : 'Collapse the menu'} aria-label={collapsed ? 'Expand the menu' : 'Collapse the menu'}>
            {collapsed ? <ChevronsRight size={15} /> : <><ChevronsLeft size={15} /> Collapse</>}
          </button>
        </div>
      </aside>

      <div className="shell-main">
        <header className="topbar">
          <Link to="/" className="lg:hidden shrink-0" aria-label="Vector Strike Range">
            <RangeMark size={30} />
          </Link>
          {page && (
            <h1 className="topbar-title hidden sm:flex">
              <page.icon size={16} />
              {page.label}
            </h1>
          )}
          {MOCK && (
            <span className="chip warn hidden md:inline-flex" title="VITE_MOCK=1: every number on this site is fixture data">MOCK DATA</span>
          )}
          <div className="ml-auto flex items-center gap-1.5 sm:gap-2 min-w-0">
            <RangeStatus />
            <button className="search-btn" onClick={() => setPalette(true)} aria-label="Search">
              <Search size={15} />
              <span className="hidden md:inline">Search</span>
              <kbd className="hidden md:inline">{isMac() ? '⌘' : 'Ctrl'} K</kbd>
            </button>
            <button className="btn-range ghost sm" onClick={toggle} aria-label={theme === 'dark' ? 'Switch to light theme' : 'Switch to dark theme'} title="Theme">
              {theme === 'dark' ? <Sun size={15} /> : <Moon size={15} />}
            </button>
            <UserButton />
            <button className="btn-range ghost sm lg:hidden" onClick={() => setDrawer(true)} aria-label="Menu" aria-expanded={drawer}>
              <Menu size={17} />
            </button>
          </div>
        </header>

        <main className={`shell-content${bleed ? ' bleed' : ''}`}>{children}</main>

        <nav className="tabbar lg:hidden" aria-label="Quick">
          {NAV.filter(n => n.tab).map(n => (
            <NavLink key={n.to} to={n.to} end={n.end}>
              <n.icon size={19} />
              <span>{n.short ?? n.label}</span>
            </NavLink>
          ))}
        </nav>
      </div>

      {drawer && (
        <div className="nav-scrim lg:hidden" onClick={() => setDrawer(false)}>
          <aside className="nav-drawer" onClick={e => e.stopPropagation()} aria-label="Menu">
            <div className="flex items-center justify-between px-3 h-14 border-b border-[var(--line)]">
              <RangeLockup size={32} />
              <button className="btn-range ghost sm" onClick={() => setDrawer(false)} aria-label="Close menu"><X size={16} /></button>
            </div>
            <div className="p-2 flex-1 overflow-y-auto"><RailLinks collapsed={false} onPick={() => setDrawer(false)} /></div>
          </aside>
        </div>
      )}

      {palette && <CommandPalette onClose={() => setPalette(false)} />}
    </div>
  )
}
