import { useState } from 'react'
import { Link, NavLink, useLocation } from 'react-router-dom'
import { Activity, Briefing, Calculator, Carrier, LogOut, Menu, Moon, Plus, Rankings, Sun, X, type IconComponent } from '@icons'
import { MOCK } from '../api'
import { useAuth } from '../context/AuthContext'
import { useTheme } from '../context/ThemeContext'
import { Lens } from './Lens'

const NAV: { to: string; label: string; icon: IconComponent; end?: boolean }[] = [
  { to: '/', label: 'Live', icon: Activity, end: true },
  { to: '/greenie', label: 'Greenie board', icon: Carrier },
  { to: '/results', label: 'Results', icon: Briefing },
  { to: '/leaderboards', label: 'Leaderboards', icon: Rankings },
  { to: '/spawn', label: 'Spawn', icon: Plus },
  { to: '/calc', label: 'Calculators', icon: Calculator },
]

function DiscordMark() {
  return (
    <svg width="14" height="14" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
      <path d="M20.3 4.4A19.8 19.8 0 0 0 15.4 3l-.6 1.3a18.3 18.3 0 0 0-5.6 0L8.6 3a19.7 19.7 0 0 0-4.9 1.5C.6 9.1-.3 13.7.1 18.2a19.9 19.9 0 0 0 6 3l1.3-2a12.9 12.9 0 0 1-2-1l.5-.4a14.2 14.2 0 0 0 12.2 0l.5.4-2 1 1.3 2a19.8 19.8 0 0 0 6-3c.5-5.2-.8-9.7-3.6-13.8zM8 15.4c-1.2 0-2.2-1.1-2.2-2.4s1-2.4 2.2-2.4 2.2 1.1 2.2 2.4-1 2.4-2.2 2.4zm8 0c-1.2 0-2.2-1.1-2.2-2.4s1-2.4 2.2-2.4 2.2 1.1 2.2 2.4-1 2.4-2.2 2.4z" />
    </svg>
  )
}

export function TopBar() {
  const { me, login, logout } = useAuth()
  const { theme, toggle } = useTheme()
  const [open, setOpen] = useState(false)
  const loc = useLocation()
  const [lastPath, setLastPath] = useState(loc.pathname)
  if (lastPath !== loc.pathname) {
    setLastPath(loc.pathname)
    setOpen(false)
  }

  const links = NAV.map(n => (
    <NavLink
      key={n.to}
      to={n.to}
      end={n.end}
      className={({ isActive }) =>
        `flex items-center gap-2 px-3 h-full text-[13px] font-medium border-b-2 transition-colors ${
          isActive ? 'border-[var(--ball)] text-[var(--chalk)]' : 'border-transparent text-[var(--haze)] hover:text-[var(--chalk)]'
        }`
      }
    >
      <n.icon size={15} />
      {n.label}
    </NavLink>
  ))

  return (
    <header className="sticky top-0 z-40 border-b border-[var(--line)]" style={{ background: 'color-mix(in srgb, var(--ink) 92%, transparent)', backdropFilter: 'blur(8px)' }}>
      <div className="wrap flex items-stretch h-14 gap-2 sm:gap-4">
        <Link to="/" className="flex items-center gap-2.5 shrink-0" aria-label="Vector Strike Range, live">
          <Lens size={30} />
          <span className="flex flex-col leading-none">
            <span className="caps" style={{ fontSize: 9, letterSpacing: '0.24em' }}>Vector Strike</span>
            <span className="display" style={{ fontSize: 22, color: 'var(--chalk)' }}>Range</span>
          </span>
        </Link>
        {MOCK && (
          <span className="chip warn self-center hidden sm:inline-flex" title="VITE_MOCK=1: every number on this site is fixture data">
            MOCK DATA
          </span>
        )}
        <nav className="hidden lg:flex items-stretch ml-2" aria-label="Main">
          {links}
        </nav>
        <div className="ml-auto flex items-center gap-1 sm:gap-2 shrink-0">
          <button className="btn-range ghost sm" onClick={toggle} aria-label={theme === 'dark' ? 'Switch to light theme' : 'Switch to dark theme'} title="Theme">
            {theme === 'dark' ? <Sun size={15} /> : <Moon size={15} />}
          </button>
          {me?.logged_in ? (
            <>
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
            </>
          ) : (
            <button className="btn-range sm" onClick={login}>
              <DiscordMark /> Log in
            </button>
          )}
          <button className="btn-range ghost sm lg:hidden" onClick={() => setOpen(o => !o)} aria-expanded={open} aria-label="Menu">
            {open ? <X size={16} /> : <Menu size={16} />}
          </button>
        </div>
      </div>
      {open && (
        <nav className="lg:hidden border-t border-[var(--line)] wrap py-1 flex flex-col [&>a]:h-10 [&>a]:border-b-0 [&>a]:border-l-2 [&>a]:px-3" aria-label="Main">
          {links}
          {me?.logged_in && (
            <NavLink to="/me" className="flex items-center gap-2 px-3 h-10 text-[13px] text-[var(--haze)] sm:hidden">
              My record
            </NavLink>
          )}
        </nav>
      )}
    </header>
  )
}
