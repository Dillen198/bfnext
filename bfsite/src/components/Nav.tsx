import { useEffect, useState } from 'react'
import { Menu, X, Wiki, Sitrep } from '@icons'
import { links } from '../config/campaign'
import ThemeToggle from './ThemeToggle'
import LogoMark from './LogoMark'

const SECTIONS = [
  { href: '#situation', label: 'Situation' },
  { href: '#mission', label: 'Mission' },
  { href: '#join', label: 'Join' },
  { href: '#intel', label: 'Intel' },
  { href: '#community', label: 'Community' },
]

export default function Nav() {
  const [open, setOpen] = useState(false)
  const [scrolled, setScrolled] = useState(false)

  useEffect(() => {
    const onScroll = () => setScrolled(window.scrollY > 40)
    onScroll()
    window.addEventListener('scroll', onScroll, { passive: true })
    return () => window.removeEventListener('scroll', onScroll)
  }, [])

  const solid = scrolled || open
  return (
    // Over the hero the bar is transparent on a dark picture, so it keeps the
    // dark palette in either theme until it turns solid.
    <header className={`nav${solid ? ' is-solid' : ' theme-locked-dark'}`}>
      <div className="wrap nav-bar">
        <a href="#top" className="nav-brand" onClick={() => setOpen(false)}>
          <LogoMark size={40} alt="" />
          <div>
            <div className="nav-brand-name">Vector Strike</div>
            <div className="nav-brand-sub">Persistent DCS campaign</div>
          </div>
        </a>

        <nav className="nav-links" aria-label="Sections">
          {SECTIONS.map((s, i) => (
            <a key={s.href} href={s.href} className="nav-link">
              <span className="n">{i + 1}</span>
              {s.label}
            </a>
          ))}
        </nav>

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

      {open && (
        <div className="nav-drawer">
          <div className="wrap">
            {SECTIONS.map((s, i) => (
              <a key={s.href} href={s.href} className="nav-link" onClick={() => setOpen(false)}>
                <span className="n">{i + 1}</span>
                {s.label}
              </a>
            ))}
            <div className="nav-drawer-actions">
              <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn vs-btn-sm">Join Discord</a>
              <a href={links.dashboard} className="vs-btn vs-btn-outline vs-btn-sm"><Sitrep size={13} />Dashboard</a>
              <a href={links.wiki} className="vs-btn vs-btn-ghost vs-btn-sm"><Wiki size={13} />Wiki</a>
            </div>
          </div>
        </div>
      )}
    </header>
  )
}
