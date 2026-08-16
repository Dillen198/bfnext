import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { campaign } from '../config/campaign'
import { Users, Target, Crosshair, Flag } from 'lucide-react'
import Reveal from './Reveal'

export default function AboutSection() {
  const { data: stats } = useQuery({
    queryKey: ['site-stats'],
    queryFn: api.stats,
    refetchInterval: 60_000,
  })

  const statBoxes = [
    { icon: Users,    label: 'Total Pilots',    value: stats?.total_pilots   ?? '—', color: 'var(--blue)' },
    { icon: Flag,     label: 'Campaigns Run',   value: stats?.total_rounds   ?? '—', color: 'var(--accent)' },
    { icon: Target,   label: 'Objectives',      value: stats?.objective_count ?? '—', color: '#f59e0b' },
    { icon: Crosshair,label: 'Total Kills',      value: stats?.total_kills    ?? '—', color: 'var(--accent)' },
  ]

  return (
    <section id="about" style={{ background: 'var(--bg)', padding: '6rem 0' }}>
      <div className="max-w-7xl mx-auto px-6">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-16 items-center">

          {/* Left — text */}
          <Reveal>
            <div className="vs-section-label mb-4">About the Campaign</div>
            <h2
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: 'clamp(2.5rem, 6vw, 4rem)',
                color: 'var(--text)',
                letterSpacing: '0.06em',
                lineHeight: 0.95,
                margin: '0 0 1.5rem 0',
              }}
            >
              THE WAR NEVER<br />
              <span style={{ color: 'var(--accent)' }}>STOPS.</span>
            </h2>
            <p style={{ fontSize: '1rem', color: 'var(--text-muted)', lineHeight: 1.75, marginBottom: '1.5rem' }}>
              {campaign.description}
            </p>
            <p style={{ fontSize: '0.9rem', color: 'var(--text-dim)', lineHeight: 1.7 }}>
              The campaign runs 24/7 on a dedicated server. Territory can change hands at any time.
              Miss a session and you might come back to find your home airbase under new management.
            </p>
            <div className="vs-divider" style={{ marginTop: '2rem' }} />
            <div className="mt-4 flex items-center gap-3">
              <div style={{ width: '3px', height: '36px', background: 'var(--accent)', borderRadius: '1px' }} />
              <blockquote style={{ margin: 0, fontSize: '0.9rem', color: 'var(--text-muted)', fontStyle: 'italic', lineHeight: 1.6 }}>
                "Best persistent campaign on DCS. Every session feels like it actually matters."
              </blockquote>
            </div>
          </Reveal>

          {/* Right — live stats grid */}
          <div className="grid grid-cols-2 gap-4">
            {statBoxes.map((s, i) => (
              <Reveal key={s.label} delay={i * 100} className="stat-box" style={{ borderLeftColor: s.color }}>
                <s.icon size={16} style={{ color: s.color, marginBottom: '0.75rem' }} />
                <div
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '2.5rem',
                    color: 'var(--text)',
                    letterSpacing: '0.04em',
                    lineHeight: 1,
                    marginBottom: '0.3rem',
                  }}
                >
                  {typeof s.value === 'number' ? s.value.toLocaleString() : s.value}
                </div>
                <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.2em', textTransform: 'uppercase' }}>
                  {s.label}
                </div>
              </Reveal>
            ))}
            <div
              className="col-span-2 flex items-center gap-2 px-3 py-2"
              style={{ background: 'rgba(77,124,15,0.06)', border: '1px solid rgba(77,124,15,0.15)', borderRadius: '2px' }}
            >
              <span style={{ width: '6px', height: '6px', borderRadius: '50%', background: '#4ade80', flexShrink: 0, animation: 'vs-pulse 1.5s ease-in-out infinite' }} />
              <span style={{ fontFamily: "'JetBrains Mono', monospace", fontSize: '0.68rem', color: 'var(--text-muted)', letterSpacing: '0.05em' }}>
                Stats updated live from {campaign.server}
              </span>
            </div>
          </div>
        </div>
      </div>
    </section>
  )
}
