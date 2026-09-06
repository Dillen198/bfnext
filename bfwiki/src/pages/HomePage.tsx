import { Link } from 'react-router-dom'
import { Compass, Radio, Crosshair, BookOpen } from 'lucide-react'

const FEATURES = [
  {
    icon: Compass,
    title: 'Core Gameplay',
    body: 'Objectives, capturing territory, logistics & supply, and the points/lives economy that drives the campaign.',
    to: '/gameplay/objectives',
  },
  {
    icon: Radio,
    title: 'F10 Menu Systems',
    body: 'Actions, JTAC, Cargo, Troop Transport, and EWR — the full radio menu toolkit available in your aircraft.',
    to: '/f10-menu/overview',
  },
  {
    icon: Crosshair,
    title: 'Advanced Tactics',
    body: 'Coordinate artillery fire missions and long-range cruise missile strikes through the JTAC system.',
    to: '/advanced/artillery',
  },
  {
    icon: BookOpen,
    title: 'Reference Tables',
    body: 'Complete chat command list, action costs, and every deployable unit with its crate requirements.',
    to: '/reference/deployables',
  },
]

export default function HomePage() {
  return (
    <div style={{ maxWidth: 900 }}>
      <div style={{ marginBottom: '2.5rem' }}>
        <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '2.6rem', letterSpacing: '0.06em', color: 'var(--text)', lineHeight: 1.1 }}>
          VECTOR STRIKE WIKI
        </div>
        <p style={{ color: 'var(--text-muted)', fontSize: '0.95rem', marginTop: '0.75rem', maxWidth: 560, lineHeight: 1.6 }}>
          Everything you need to fly, fight, and win the persistent campaign — objectives, logistics, JTAC, and the full F10 menu system.
        </p>
        <div style={{ display: 'flex', gap: 10, marginTop: '1.5rem' }}>
          <Link to="/getting-started/welcome" className="vs-btn">GET STARTED</Link>
          <a href="https://discord.gg/wAsBEfse" target="_blank" rel="noreferrer" className="vs-btn-outline" style={{ display: 'inline-flex', alignItems: 'center', padding: '6px 14px', borderRadius: 2, fontSize: '0.72rem', letterSpacing: '0.12em', fontFamily: "'Bebas Neue', sans-serif", textDecoration: 'none' }}>
            JOIN THE DISCORD
          </a>
        </div>
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))', gap: 14 }}>
        {FEATURES.map(f => (
          <Link
            key={f.title}
            to={f.to}
            className="vs-card"
            style={{ padding: '1.1rem 1.25rem', textDecoration: 'none', display: 'block' }}
          >
            <f.icon size={18} style={{ color: 'var(--accent)', marginBottom: 8 }} />
            <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1rem', letterSpacing: '0.08em', color: 'var(--text)', marginBottom: 6 }}>
              {f.title.toUpperCase()}
            </div>
            <div style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.55 }}>{f.body}</div>
          </Link>
        ))}
      </div>
    </div>
  )
}
