import { ExternalLink, Users, Zap } from 'lucide-react'
import PageHeader from '../components/PageHeader'
import { campaign } from '../config/campaign'

export default function AboutPage() {
  return (
    <div className="flex-1 overflow-y-auto" style={{ background: 'var(--bg)' }}>
      <PageHeader
        title="ABOUT"
        subtitle="Campaign system & credits"
      />

      <div className="p-6 space-y-8 max-w-3xl">

        {/* ── Fowl Engine ── */}
        <section>
          <div
            className="mb-4"
            style={{
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: '1.1rem',
              letterSpacing: '0.18em',
              color: 'var(--text)',
              borderLeft: '3px solid var(--accent)',
              paddingLeft: '0.75rem',
            }}
          >
            FOWL ENGINE
          </div>

          <div className="vs-card p-5 space-y-3">
            <div className="flex items-center gap-2 mb-1">
              <Zap size={16} style={{ color: 'var(--accent)' }} />
              <span
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '0.95rem',
                  letterSpacing: '0.14em',
                  color: 'var(--text)',
                }}
              >
                Dynamic Persistent Campaign System
              </span>
            </div>

            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.7 }}>
              Fowl Engine is a persistent, multiplayer dynamic campaign system for DCS World.
              It enables full-scale warfare campaigns with territory control, logistics management,
              objective capturing, and coordinated combined arms operations — all tracked in
              real time across every sortie.
            </p>

            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.7 }}>
              Every action shapes the battlefield: supply convoys can be interdicted, FARPs
              destroyed, troops deployed, and territory flipped. Campaigns persist across
              sessions with full stat tracking, kill feeds, and leaderboards.
            </p>

            <div className="flex flex-wrap gap-2 pt-1">
              {[
                'Territory Control',
                'Logistics',
                'Cargo Transport',
                'Troop Deployments',
                'JTAC',
                'EWR',
                'Kill Tracking',
                'Commander System',
                'Pilot Lives',
                'F10 Menus',
                'Persistent Stats',
                'Combined Arms',
              ].map(tag => (
                <span
                  key={tag}
                  style={{
                    fontSize: '0.6rem',
                    letterSpacing: '0.12em',
                    textTransform: 'uppercase',
                    color: 'var(--accent)',
                    border: '1px solid var(--accent)',
                    padding: '2px 8px',
                    borderRadius: '2px',
                    opacity: 0.85,
                  }}
                >
                  {tag}
                </span>
              ))}
            </div>

            <div className="flex gap-4 pt-2">
              <a
                href={campaign.discord}
                target="_blank"
                rel="noreferrer"
                className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                style={{ fontSize: '0.68rem', color: '#5865F2', textDecoration: 'none', letterSpacing: '0.1em' }}
              >
                <svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
                  <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
                </svg>
                JOIN DISCORD
              </a>
              <a
                href="https://docs.google.com/presentation/d/1EAOe0iK-1s6i0UV5ObxSD86gGBj1Ixz6FOotQn5XPdc/edit#slide=id.g2b6a346170f_1_35"
                target="_blank"
                rel="noreferrer"
                className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                style={{ fontSize: '0.68rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
              >
                <ExternalLink size={11} />
                CAMPAIGN PRESENTATION
              </a>
            </div>
          </div>
        </section>

        {/* ── Special Thanks ── */}
        <section>
          <div
            className="mb-4"
            style={{
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: '1.1rem',
              letterSpacing: '0.18em',
              color: 'var(--text)',
              borderLeft: '3px solid var(--accent)',
              paddingLeft: '0.75rem',
            }}
          >
            SPECIAL THANKS
          </div>

          <div className="vs-card p-5">
            <div className="flex items-start gap-4">
              {/* Eagle Dynamics logo */}
              <img
                src="https://www.digitalcombatsimulator.com/images/logos/Eagle_Dynamics.png"
                alt="Eagle Dynamics"
                style={{ width: 220, height: 'auto', flexShrink: 0, objectFit: 'contain' }}
              />
              <div>
                <div
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '0.95rem',
                    letterSpacing: '0.14em',
                    color: 'var(--text)',
                    marginBottom: '0.35rem',
                  }}
                >
                  Eagle Dynamics
                </div>
                <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: '0.5rem' }}>
                  Developers of DCS World — the premier military flight simulator that
                  Fowl Engine is built upon. Eagle Dynamics provides the platform that
                  makes this campaign system possible.
                </p>
                <a
                  href="https://www.digitalcombatsimulator.com"
                  target="_blank"
                  rel="noreferrer"
                  className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                  style={{ fontSize: '0.65rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
                >
                  <ExternalLink size={10} />
                  DIGITALCOMBATSIMULATOR.COM
                </a>
              </div>
            </div>
          </div>
        </section>

        {/* ── Credits ── */}
        <section>
          <div
            className="mb-4"
            style={{
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: '1.1rem',
              letterSpacing: '0.18em',
              color: 'var(--text)',
              borderLeft: '3px solid var(--accent)',
              paddingLeft: '0.75rem',
            }}
          >
            CREDITS
          </div>

          <div className="vs-card p-5">
            <div className="flex items-center gap-2 mb-3">
              <Users size={14} style={{ color: 'var(--accent)' }} />
              <span
                style={{
                  fontSize: '0.65rem',
                  letterSpacing: '0.14em',
                  textTransform: 'uppercase',
                  color: 'var(--text-dim)',
                }}
              >
                Developed &amp; Customized By
              </span>
            </div>
            <div className="space-y-2">
              {['No15 | KillerDog', '[.ID] EagleEye'].map(name => (
                <div
                  key={name}
                  className="flex items-center gap-3"
                  style={{
                    padding: '0.5rem 0.75rem',
                    background: 'rgba(77,124,15,0.05)',
                    border: '1px solid rgba(77,124,15,0.15)',
                    borderRadius: '2px',
                  }}
                >
                  <div
                    style={{
                      width: 6,
                      height: 6,
                      borderRadius: '50%',
                      background: 'var(--accent)',
                      flexShrink: 0,
                    }}
                  />
                  <span
                    style={{
                      fontFamily: "'Bebas Neue', sans-serif",
                      fontSize: '0.85rem',
                      letterSpacing: '0.1em',
                      color: 'var(--text)',
                    }}
                  >
                    {name}
                  </span>
                </div>
              ))}
            </div>
          </div>
        </section>

      </div>
    </div>
  )
}
