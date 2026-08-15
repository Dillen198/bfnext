import { ExternalLink, Zap } from 'lucide-react'

export default function SpecialThanksSection() {
  return (
    <section id="thanks" style={{ background: '#060606', padding: '5rem 0', borderTop: '1px solid var(--border)' }}>
      <div className="max-w-7xl mx-auto px-6">

        <div className="vs-section-label mb-4">Special Thanks</div>
        <h2
          style={{
            fontFamily: "'Bebas Neue', sans-serif",
            fontSize: 'clamp(2rem, 4vw, 3rem)',
            color: 'var(--text)',
            letterSpacing: '0.06em',
            lineHeight: 1,
            marginBottom: '3rem',
          }}
        >
          BUILT ON THE SHOULDERS<br />
          <span style={{ color: 'var(--accent)' }}>OF GIANTS.</span>
        </h2>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">

          {/* Fowl Engine */}
          <div
            style={{
              background: '#0d0d0d',
              border: '1px solid var(--border)',
              borderLeft: '3px solid var(--accent)',
              borderRadius: '3px',
              padding: '1.75rem',
              display: 'flex',
              flexDirection: 'column',
              gap: '1rem',
            }}
          >
            <div className="flex items-center gap-2">
              <Zap size={18} style={{ color: 'var(--accent)', flexShrink: 0 }} />
              <div
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1.1rem',
                  letterSpacing: '0.14em',
                  color: 'var(--text)',
                }}
              >
                Fowl Engine
              </div>
            </div>
            <p style={{ fontSize: '0.82rem', color: 'var(--text-dim)', lineHeight: 1.65, margin: 0 }}>
              The dynamic persistent campaign engine powering Vector Strike.
              Fowl Engine manages territory control, logistics, objective capture,
              cargo transport, troop deployments, kill tracking, and real-time
              stat synchronisation — all running live inside DCS World.
            </p>
            <div className="flex flex-wrap gap-1.5">
              {['Territory Control', 'Logistics', 'Cargo Transport', 'Troop Deployments', 'JTAC', 'EWR', 'Kill Tracking', 'Commander System', 'Pilot Lives', 'F10 Menus', 'Persistent Stats', 'Combined Arms'].map(tag => (
                <span
                  key={tag}
                  style={{
                    fontSize: '0.58rem',
                    letterSpacing: '0.1em',
                    textTransform: 'uppercase',
                    color: 'var(--accent)',
                    border: '1px solid rgba(77,124,15,0.4)',
                    padding: '2px 7px',
                    borderRadius: '2px',
                  }}
                >
                  {tag}
                </span>
              ))}
            </div>
          </div>

          {/* Eagle Dynamics */}
          <div
            style={{
              background: '#0d0d0d',
              border: '1px solid var(--border)',
              borderRadius: '3px',
              padding: '1.75rem',
              display: 'flex',
              flexDirection: 'column',
              gap: '1rem',
            }}
          >
            <img
              src="https://www.digitalcombatsimulator.com/images/logos/Eagle_Dynamics.png"
              alt="Eagle Dynamics"
              style={{ width: 220, height: 'auto', objectFit: 'contain' }}
            />

            <div>
              <div
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1.1rem',
                  letterSpacing: '0.14em',
                  color: 'var(--text)',
                  marginBottom: '0.5rem',
                }}
              >
                Eagle Dynamics
              </div>
              <p style={{ fontSize: '0.82rem', color: 'var(--text-dim)', lineHeight: 1.65, margin: '0 0 0.75rem 0' }}>
                Developers of DCS World — the premier military flight simulator
                that Vector Strike and Fowl Engine are built upon.
              </p>
              <a
                href="https://www.digitalcombatsimulator.com"
                target="_blank"
                rel="noopener noreferrer"
                style={{
                  display: 'inline-flex',
                  alignItems: 'center',
                  gap: '0.3rem',
                  fontSize: '0.68rem',
                  color: 'var(--accent)',
                  textDecoration: 'none',
                  letterSpacing: '0.1em',
                  textTransform: 'uppercase',
                }}
                onMouseEnter={e => (e.currentTarget.style.opacity = '0.75')}
                onMouseLeave={e => (e.currentTarget.style.opacity = '1')}
              >
                <ExternalLink size={10} />
                digitalcombatsimulator.com
              </a>
            </div>
          </div>

          {/* 61st ATP — server host */}
          <div
            style={{
              background: '#0d0d0d',
              border: '1px solid var(--border)',
              borderLeft: '3px solid var(--accent)',
              borderRadius: '3px',
              padding: '1.75rem',
              display: 'flex',
              flexDirection: 'column',
              gap: '1rem',
            }}
          >
            <div className="flex items-center gap-3">
              <img
                src="/hacker.jpg"
                alt="Hacker"
                style={{ width: 56, height: 56, borderRadius: '8px', objectFit: 'cover', flexShrink: 0 }}
              />
              <div
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1.1rem',
                  letterSpacing: '0.14em',
                  color: 'var(--text)',
                }}
              >
                61st ATP
              </div>
            </div>
            <p style={{ fontSize: '0.82rem', color: 'var(--text-dim)', lineHeight: 1.65, margin: 0 }}>
              A persistent, 24/7 campaign lives or dies on having a machine that never goes offline —
              and Vector Strike has one because <strong style={{ color: 'var(--text)' }}>Hacker</strong>, founder of{' '}
              <strong style={{ color: 'var(--text)' }}>61st ATP — Arctic Terns &amp; Penguins</strong>, generously
              donated the server it runs on. Every objective captured, every sortie flown, and every name on the
              leaderboard is only possible because of that support. On behalf of the whole squadron: thank you.
            </p>
          </div>

          {/* DCSServerBot */}
          <div
            style={{
              background: '#0d0d0d',
              border: '1px solid var(--border)',
              borderRadius: '3px',
              padding: '1.75rem',
              display: 'flex',
              flexDirection: 'column',
              gap: '1rem',
            }}
          >
            <img
              src="/special-k.png"
              alt="Special K"
              style={{ width: 64, height: 64, borderRadius: '8px', objectFit: 'contain' }}
            />

            <div>
              <div
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1.1rem',
                  letterSpacing: '0.14em',
                  color: 'var(--text)',
                  marginBottom: '0.5rem',
                }}
              >
                DCSServerBot
              </div>
              <p style={{ fontSize: '0.82rem', color: 'var(--text-dim)', lineHeight: 1.65, margin: '0 0 0.75rem 0' }}>
                Built by <strong style={{ color: 'var(--text)' }}>Special K</strong>, DCSServerBot is the Discord
                framework that runs our server bot — mission control, player management, and the live campaign
                integration that ties Vector Strike's Discord presence to the fight in the air.
              </p>
              <a
                href="https://github.com/Special-K-s-Flightsim-Bots/DCSServerBot"
                target="_blank"
                rel="noopener noreferrer"
                style={{
                  display: 'inline-flex',
                  alignItems: 'center',
                  gap: '0.3rem',
                  fontSize: '0.68rem',
                  color: 'var(--accent)',
                  textDecoration: 'none',
                  letterSpacing: '0.1em',
                  textTransform: 'uppercase',
                }}
                onMouseEnter={e => (e.currentTarget.style.opacity = '0.75')}
                onMouseLeave={e => (e.currentTarget.style.opacity = '1')}
              >
                <ExternalLink size={10} />
                github.com/DCSServerBot
              </a>
            </div>
          </div>

        </div>

        {/* Customized by */}
        <div
          className="mt-10 flex flex-wrap items-center gap-3"
          style={{ borderTop: '1px solid var(--border)', paddingTop: '2rem' }}
        >
          <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
            Developed &amp; Customized by
          </span>
          {['No15 | KillerDog', '[.ID] EagleEye'].map(name => (
            <span
              key={name}
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '0.85rem',
                letterSpacing: '0.1em',
                color: 'var(--text)',
                background: 'rgba(77,124,15,0.07)',
                border: '1px solid rgba(77,124,15,0.2)',
                padding: '3px 10px',
                borderRadius: '2px',
              }}
            >
              {name}
            </span>
          ))}
        </div>

      </div>
    </section>
  )
}
