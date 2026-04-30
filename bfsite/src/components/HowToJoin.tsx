import { campaign } from '../config/campaign'
import { Zap } from 'lucide-react'

export default function HowToJoin() {
  return (
    <section id="join" style={{ background: 'var(--bg)', padding: '6rem 0', borderTop: '1px solid var(--border)' }}>
      <div className="max-w-7xl mx-auto px-6">

        {/* Header */}
        <div className="mb-14">
          <div className="vs-section-label mb-4">Ready to fly?</div>
          <h2
            style={{
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: 'clamp(2.5rem, 6vw, 3.5rem)',
              color: 'var(--text)',
              letterSpacing: '0.08em',
              margin: 0,
            }}
          >
            HOW TO <span style={{ color: 'var(--accent)' }}>JOIN</span>
          </h2>
        </div>

        {/* Steps */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-14">
          {campaign.joinSteps.map((step, i) => (
            <div key={i} className="relative">
              {/* Connector line (not on last item) */}
              {i < campaign.joinSteps.length - 1 && (
                <div
                  className="hidden lg:block absolute top-8 left-[calc(100%+0px)] w-full h-px"
                  style={{ background: 'linear-gradient(to right, var(--accent), transparent)', opacity: 0.3, zIndex: 0 }}
                />
              )}

              <div style={{ position: 'relative', zIndex: 1 }}>
                {/* Step number */}
                <div
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '3.5rem',
                    color: 'var(--accent)',
                    lineHeight: 1,
                    marginBottom: '0.75rem',
                    opacity: 0.9,
                  }}
                >
                  {step.number}
                </div>

                {/* Red bar */}
                <div style={{ width: '32px', height: '2px', background: 'var(--accent)', marginBottom: '0.85rem', opacity: 0.6 }} />

                <h3
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '1.1rem',
                    letterSpacing: '0.12em',
                    color: 'var(--text)',
                    margin: '0 0 0.6rem 0',
                  }}
                >
                  {step.title}
                </h3>

                <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', lineHeight: 1.65, margin: 0 }}>
                  {step.description}
                </p>
              </div>
            </div>
          ))}
        </div>

        {/* CTA block */}
        <div
          style={{
            background: '#0d0d0d',
            border: '1px solid var(--border)',
            borderLeft: '4px solid var(--accent)',
            borderRadius: '2px',
            padding: '2rem 2.5rem',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'space-between',
            flexWrap: 'wrap',
            gap: '1.5rem',
          }}
        >
          <div>
            <h3
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1.6rem',
                letterSpacing: '0.1em',
                color: 'var(--text)',
                margin: '0 0 0.3rem 0',
              }}
            >
              READY TO JOIN THE FIGHT?
            </h3>
            <p style={{ margin: 0, fontSize: '0.85rem', color: 'var(--text-muted)' }}>
              All pilot skill levels welcome. Full briefings and doctrine available in Discord.
            </p>
          </div>
          <a
            href={campaign.discord}
            target="_blank"
            rel="noopener noreferrer"
            className="vs-btn"
            style={{ fontSize: '1rem', padding: '0.7rem 2rem', flexShrink: 0 }}
          >
            <Zap size={16} />
            JOIN DISCORD NOW
          </a>
        </div>
      </div>
    </section>
  )
}
