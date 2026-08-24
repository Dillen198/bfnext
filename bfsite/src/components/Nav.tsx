import React from 'react'
import { campaign } from '../config/campaign'
import { Zap, Menu, X, BookOpen } from 'lucide-react'

const links = [
  { href: '#about',    label: 'ABOUT' },
  { href: '#features', label: 'FEATURES' },
  { href: '#join',     label: 'HOW TO JOIN' },
  { href: '#stats',    label: 'LIVE STATS' },
  { href: '#manual',   label: 'MANUAL' },
]

export default function Nav() {
  const [open, setOpen] = React.useState(false)
  const [scrolled, setScrolled] = React.useState(false)

  React.useEffect(() => {
    const onScroll = () => setScrolled(window.scrollY > 40)
    window.addEventListener('scroll', onScroll)
    return () => window.removeEventListener('scroll', onScroll)
  }, [])

  return (
    <header
      className="fixed top-0 inset-x-0 z-50 transition-all duration-300"
      style={{
        background: scrolled ? 'rgba(10,10,10,0.95)' : 'transparent',
        backdropFilter: scrolled ? 'blur(12px)' : 'none',
        borderBottom: scrolled ? '1px solid var(--border)' : '1px solid transparent',
      }}
    >
      <div className="max-w-7xl mx-auto px-6 flex items-center justify-between h-16">

        {/* Logo */}
        <a href="#" className="flex items-center gap-3 no-underline">
          <div
            className="flex items-center justify-center w-9 h-9 flex-shrink-0"
            style={{
              background: 'var(--accent)',
              borderRadius: '2px',
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: '1rem',
              color: '#fff',
              letterSpacing: '0.05em',
            }}
          >
            VS
          </div>
          <div>
            <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1rem', letterSpacing: '0.2em', color: 'var(--text)', lineHeight: 1 }}>
              {campaign.name}
            </div>
            <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.18em', textTransform: 'uppercase', marginTop: '1px' }}>
              Dynamic DCS Campaign
            </div>
          </div>
        </a>

        {/* Desktop nav */}
        <nav className="hidden md:flex items-center gap-8">
          {links.map(l => (
            <a
              key={l.href}
              href={l.href}
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '0.82rem',
                letterSpacing: '0.18em',
                color: 'var(--text-muted)',
                textDecoration: 'none',
                transition: 'color 0.15s',
              }}
              onMouseEnter={e => (e.currentTarget.style.color = 'var(--text)')}
              onMouseLeave={e => (e.currentTarget.style.color = 'var(--text-muted)')}
            >
              {l.label}
            </a>
          ))}
        </nav>

        {/* CTA */}
        <div className="hidden md:flex items-center gap-3">
          <a href={campaign.dashboardUrl} className="vs-btn" style={{ padding: '0.45rem 1rem', fontSize: '0.78rem' }}>
            <Zap size={12} />
            OPS DASHBOARD
          </a>
          <a href={campaign.wikiUrl} className="vs-btn vs-btn-outline" style={{ padding: '0.45rem 1rem', fontSize: '0.78rem' }}>
            <BookOpen size={12} />
            WIKI
          </a>
          <a
            href={campaign.discord}
            target="_blank"
            rel="noopener noreferrer"
            className="vs-btn vs-btn-outline"
            style={{ padding: '0.45rem 1rem', fontSize: '0.78rem' }}
          >
            JOIN DISCORD
          </a>
        </div>

        {/* Mobile hamburger */}
        <button
          className="md:hidden"
          onClick={() => setOpen(o => !o)}
          style={{ background: 'none', border: 'none', color: 'var(--text)', cursor: 'pointer', padding: '0.25rem' }}
        >
          {open ? <X size={22} /> : <Menu size={22} />}
        </button>
      </div>

      {/* Mobile menu */}
      {open && (
        <div style={{ background: 'rgba(10,10,10,0.98)', borderBottom: '1px solid var(--border)', padding: '1rem 1.5rem' }}>
          {links.map(l => (
            <a
              key={l.href}
              href={l.href}
              onClick={() => setOpen(false)}
              style={{
                display: 'block',
                padding: '0.75rem 0',
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1rem',
                letterSpacing: '0.18em',
                color: 'var(--text-muted)',
                textDecoration: 'none',
                borderBottom: '1px solid var(--border)',
              }}
            >
              {l.label}
            </a>
          ))}
          <div className="flex gap-3 mt-4">
            <a href={campaign.discord} target="_blank" rel="noopener noreferrer" className="vs-btn" style={{ fontSize: '0.82rem' }}>JOIN DISCORD</a>
            <a href={campaign.dashboardUrl} className="vs-btn vs-btn-outline" style={{ fontSize: '0.82rem' }}>DASHBOARD</a>
            <a href={campaign.wikiUrl} className="vs-btn vs-btn-outline" style={{ fontSize: '0.82rem' }}>WIKI</a>
          </div>
        </div>
      )}
    </header>
  )
}
