import { useEffect, useState } from 'react'
import { Link, NavLink, useLocation } from 'react-router-dom'
import { Menu, X, Wiki, Sitrep } from '@icons'
import { links } from '../config/campaign'
import ThemeToggle from './ThemeToggle'
import LogoMark from './LogoMark'
import StatusBanner from './StatusBanner'
import NotificationBell from './NotificationBell'

const PAGES = [
  { to: '/servers', label: 'Servers' },
  { to: '/join', label: 'Join' },
  { to: '/status', label: 'Status' },
  { to: '/community', label: 'Community' },
]

export default function Nav() {
  const [open, setOpen] = useState(false)
  const [scrolled, setScrolled] = useState(false)
  const { pathname } = useLocation()

  useEffect(() => {
    const onScroll = () => setScrolled(window.scrollY > 40)
    onScroll()
    window.addEventListener('scroll', onScroll, { passive: true })
    return () => window.removeEventListener('scroll', onScroll)
  }, [])

  // Only the home page has a dark picture under the bar. There it stays
  // transparent, in the dark palette, until scrolled; elsewhere it is solid.
  const solid = scrolled || open || pathname !== '/'
  const close = () => setOpen(false)
  return (
    <header className={`nav${solid ? ' is-solid' : ' theme-locked-dark'}`}>
      <div className="wrap nav-bar">
        <Link to="/" className="nav-brand" onClick={close}>
          <LogoMark size={40} alt="" />
          <div>
            <div className="nav-brand-name">Vector Strike</div>
            <div className="nav-brand-sub">Persistent DCS campaign</div>
          </div>
        </Link>

        <nav className="nav-links" aria-label="Pages">
          {PAGES.map(p => (
            <NavLink key={p.to} to={p.to} className="nav-link">{p.label}</NavLink>
          ))}
        </nav>

        <div className="nav-right">
          {/* One bell for every screen size: two copies would keep two unread counts. */}
          <NotificationBell />

          <div className="nav-actions">
            <ThemeToggle />
            <a href={links.wiki} className="vs-btn vs-btn-ghost vs-btn-sm"><Wiki size={13} />Wiki</a>
            <a href={links.dashboard} className="vs-btn vs-btn-outline vs-btn-sm"><Sitrep size={13} />Dashboard</a>
            <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn vs-btn-sm">Join Discord</a>
          </div>

          <div className="nav-burger">
            <ThemeToggle />
            <button
              type="button"
              className="icon-btn"
              aria-label={open ? 'Close menu' : 'Open menu'}
              aria-expanded={open}
              onClick={() => setOpen(o => !o)}
            >
              {open ? <X size={18} /> : <Menu size={18} />}
            </button>
          </div>
        </div>
      </div>

      {open && (
        <div className="nav-drawer">
          <div className="wrap">
            <NavLink to="/" end className="nav-link" onClick={close}>Home</NavLink>
            {PAGES.map(p => (
              <NavLink key={p.to} to={p.to} className="nav-link" onClick={close}>{p.label}</NavLink>
            ))}
            <div className="nav-drawer-actions">
              <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn vs-btn-sm">Join Discord</a>
              <a href={links.dashboard} className="vs-btn vs-btn-outline vs-btn-sm"><Sitrep size={13} />Dashboard</a>
              <a href={links.wiki} className="vs-btn vs-btn-ghost vs-btn-sm"><Wiki size={13} />Wiki</a>
            </div>
          </div>
        </div>
      )}

      <StatusBanner />
    </header>
  )
}
