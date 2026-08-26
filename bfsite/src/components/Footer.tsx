import { campaign } from '../config/campaign'
import { Zap, ExternalLink } from 'lucide-react'

export default function Footer() {
  const year = new Date().getFullYear()

  return (
    <footer style={{ background: 'var(--bg-alt2)', borderTop: '1px solid var(--border)', padding: '3rem 0 2rem' }}>
      <div className="max-w-7xl mx-auto px-6">

        {/* Top row */}
        <div className="flex flex-col md:flex-row justify-between gap-10 mb-10">

          {/* Brand */}
          <div className="max-w-sm">
            <div className="flex items-center gap-3 mb-4">
              <div
                style={{
                  background: 'var(--accent)',
                  borderRadius: '2px',
                  width: '36px',
                  height: '36px',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '0.95rem',
                  color: 'var(--accent-fg)',
                  letterSpacing: '0.05em',
                  flexShrink: 0,
                }}
              >
                VS
              </div>
              <div>
                <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1rem', letterSpacing: '0.2em', color: 'var(--text)', lineHeight: 1 }}>
                  {campaign.name}
                </div>
                <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.16em', textTransform: 'uppercase', marginTop: '2px' }}>
                  {campaign.version} · Dynamic DCS Campaign
                </div>
              </div>
            </div>
            <p style={{ fontSize: '0.82rem', color: 'var(--text-dim)', lineHeight: 1.65, margin: 0 }}>
              {campaign.tagline}
            </p>
          </div>

          {/* Links */}
          <div className="grid grid-cols-2 gap-x-16 gap-y-2">
            <div>
              <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.75rem', letterSpacing: '0.2em', color: 'var(--text-muted)', marginBottom: '0.75rem' }}>
                Navigation
              </div>
              {[
                { label: 'About', href: '#about' },
                { label: 'Features', href: '#features' },
                { label: 'How to Join', href: '#join' },
                { label: 'Live Stats', href: '#stats' },
                { label: 'Pilot Manual', href: '#manual' },
              ].map(l => (
                <a
                  key={l.href}
                  href={l.href}
                  style={{ display: 'block', fontSize: '0.82rem', color: 'var(--text-dim)', textDecoration: 'none', marginBottom: '0.4rem', transition: 'color 0.15s' }}
                  onMouseEnter={e => (e.currentTarget.style.color = 'var(--text)')}
                  onMouseLeave={e => (e.currentTarget.style.color = 'var(--text-dim)')}
                >
                  {l.label}
                </a>
              ))}
            </div>
            <div>
              <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.75rem', letterSpacing: '0.2em', color: 'var(--text-muted)', marginBottom: '0.75rem' }}>
                Links
              </div>
              {[
                { label: 'Ops Dashboard', href: campaign.dashboardUrl },
                { label: 'Discord Server', href: campaign.discord, external: true },
                { label: 'DCS World', href: 'https://www.digitalcombatsimulator.com', external: true },
              ].map(l => (
                <a
                  key={l.label}
                  href={l.href}
                  target={l.external ? '_blank' : undefined}
                  rel={l.external ? 'noopener noreferrer' : undefined}
                  style={{ display: 'flex', alignItems: 'center', gap: '0.3rem', fontSize: '0.82rem', color: 'var(--text-dim)', textDecoration: 'none', marginBottom: '0.4rem', transition: 'color 0.15s' }}
                  onMouseEnter={e => (e.currentTarget.style.color = 'var(--text)')}
                  onMouseLeave={e => (e.currentTarget.style.color = 'var(--text-dim)')}
                >
                  {l.label}
                  {l.external && <ExternalLink size={10} />}
                </a>
              ))}
            </div>
          </div>

          {/* Discord CTA */}
          <div>
            <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.75rem', letterSpacing: '0.2em', color: 'var(--text-muted)', marginBottom: '0.75rem' }}>
              Join the Community
            </div>
            <a
              href={campaign.discord}
              target="_blank"
              rel="noopener noreferrer"
              className="vs-btn"
              style={{ fontSize: '0.85rem', padding: '0.55rem 1.25rem' }}
            >
              <Zap size={14} />
              DISCORD
            </a>
          </div>
        </div>

        {/* Divider */}
        <div className="vs-divider" />

        {/* Bottom */}
        <div className="flex flex-col sm:flex-row justify-between items-center gap-3 mt-5">
          <span style={{ fontSize: '0.7rem', color: 'var(--text-dim)', letterSpacing: '0.08em' }}>
            © {year} {campaign.name} Campaign · Powered by Fowl Engine
          </span>
          <span style={{ fontSize: '0.7rem', color: 'var(--text-dim)', letterSpacing: '0.06em' }}>
            Developed &amp; Customized by No15 | KillerDog &amp; [.ID] EagleEye
          </span>
        </div>
      </div>
    </footer>
  )
}
