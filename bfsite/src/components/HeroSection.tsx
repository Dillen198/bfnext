import { campaign } from '../config/campaign'
import { Zap, ChevronDown, ArrowRight } from 'lucide-react'

export default function HeroSection() {
  return (
    <section
      id="hero"
      className="relative min-h-screen flex flex-col justify-center overflow-hidden"
      style={{ background: '#050505' }}
    >
      {/* Background image + overlay */}
      <div
        className="absolute inset-0"
        style={{
          backgroundImage: 'url(/hero.jpeg)',
          backgroundSize: 'cover',
          backgroundPosition: 'center 30%',
          backgroundRepeat: 'no-repeat',
        }}
      />
      {/* Dark overlay layers */}
      <div className="absolute inset-0" style={{ background: 'rgba(0,0,0,0.32)' }} />
      <div className="absolute inset-0" style={{ background: 'linear-gradient(to right, rgba(10,10,10,0.88) 0%, rgba(10,10,10,0.35) 60%, transparent 100%)' }} />
      <div className="absolute inset-0" style={{ background: 'linear-gradient(to top, #0a0a0a 0%, transparent 40%)' }} />

      {/* Red accent diagonal line */}
      <div
        className="absolute pointer-events-none"
        style={{
          top: 0, left: 0, right: 0, bottom: 0,
          background: 'linear-gradient(135deg, rgba(77,124,15,0.08) 0%, transparent 50%)',
        }}
      />

      {/* Corner brackets */}
      <div className="absolute top-24 left-8 w-12 h-12 pointer-events-none" style={{ borderTop: '2px solid rgba(77,124,15,0.6)', borderLeft: '2px solid rgba(77,124,15,0.6)' }} />
      <div className="absolute bottom-16 right-8 w-12 h-12 pointer-events-none" style={{ borderBottom: '2px solid rgba(77,124,15,0.3)', borderRight: '2px solid rgba(77,124,15,0.3)' }} />

      {/* Content */}
      <div className="relative max-w-7xl mx-auto px-6 pt-24 pb-16 w-full">
        <div className="max-w-3xl">

          {/* Eyebrow */}
          <div className="flex items-center gap-3 mb-6 fade-up fade-up-d1">
            <div style={{ width: '32px', height: '2px', background: 'var(--accent)' }} />
            <span className="vs-section-label">DCS World · Persistent Multiplayer Campaign</span>
          </div>

          {/* Main title */}
          <h1 className="fade-up fade-up-d2" style={{ margin: 0, lineHeight: 0.9 }}>
            <span
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: 'clamp(5rem, 14vw, 11rem)',
                color: 'var(--text)',
                letterSpacing: '0.04em',
                display: 'block',
              }}
            >
              VECTOR
            </span>
            <span
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: 'clamp(5rem, 14vw, 11rem)',
                color: 'var(--accent)',
                letterSpacing: '0.04em',
                display: 'block',
                WebkitTextStroke: '0px',
              }}
            >
              STRIKE
            </span>
          </h1>

          {/* Tagline */}
          <p
            className="mt-6 mb-8 fade-up fade-up-d3"
            style={{
              fontSize: 'clamp(1rem, 2.5vw, 1.2rem)',
              color: 'var(--text-muted)',
              fontWeight: 300,
              letterSpacing: '0.04em',
              maxWidth: '480px',
              lineHeight: 1.6,
            }}
          >
            {campaign.tagline}
          </p>

          {/* CTAs */}
          <div className="flex flex-wrap gap-4 mb-12 fade-up fade-up-d4">
            <a
              href={campaign.discord}
              target="_blank"
              rel="noopener noreferrer"
              className="vs-btn"
              style={{ fontSize: '1rem', padding: '0.75rem 2rem' }}
            >
              <Zap size={16} />
              JOIN DISCORD
            </a>
            <a
              href={campaign.dashboardUrl}
              className="vs-btn vs-btn-outline"
              style={{ fontSize: '1rem', padding: '0.75rem 2rem' }}
            >
              VIEW OPS DASHBOARD
              <ArrowRight size={15} />
            </a>
          </div>

          {/* Quick stats strip */}
          <div className="flex flex-wrap gap-6">
            {[
              { label: 'Theaters', value: 'Multi' },
              { label: 'Persistence', value: '24/7' },
              { label: 'Factions', value: '2' },
              { label: 'Mode', value: 'PvP' },
            ].map(s => (
              <div key={s.label}>
                <div
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '1.6rem',
                    color: 'var(--text)',
                    letterSpacing: '0.05em',
                    lineHeight: 1,
                  }}
                >
                  {s.value}
                </div>
                <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.2em', textTransform: 'uppercase', marginTop: '2px' }}>
                  {s.label}
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Scroll indicator */}
      <div
        className="absolute bottom-8 left-1/2 -translate-x-1/2 flex flex-col items-center gap-2"
        style={{ animation: 'scroll-indicator 2s ease-in-out infinite' }}
      >
        <ChevronDown size={20} style={{ color: 'var(--accent)', opacity: 0.7 }} />
      </div>
    </section>
  )
}
