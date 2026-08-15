import { ExternalLink, Server, Users, Zap } from 'lucide-react'
import PageHeader from '../components/PageHeader'
import { campaign } from '../config/campaign'

// Role badge colour mapping
const ROLE_COLORS: Record<string, string> = {
  Creator:              '#f59e0b',
  'Lead Developer':     'var(--accent)',
  'Tester & Contributor': '#64748b',
}

export default function AboutPage() {
  return (
    <div className="flex-1 overflow-y-auto" style={{ background: 'var(--bg)' }}>
      <PageHeader
        title="ABOUT"
        sub="Campaign system & credits"
      />

      <div className="p-6 space-y-8 max-w-3xl">

        {/* ── Fowl Engine ── */}
        <section>
          <SectionHeading>FOWL ENGINE</SectionHeading>

          <div className="vs-card p-5 space-y-3">
            <div className="flex items-center gap-2 mb-1">
              <Zap size={16} style={{ color: 'var(--accent)' }} />
              <span className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)' }}>
                Dynamic Persistent Campaign System for DCS World
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
                'Territory Control', 'Logistics', 'Cargo Transport', 'Troop Deployments',
                'JTAC', 'EWR', 'Kill Tracking', 'Commander System',
                'Pilot Lives', 'F10 Menus', 'Persistent Stats', 'Combined Arms',
              ].map(tag => (
                <span
                  key={tag}
                  style={{
                    fontSize: '0.6rem', letterSpacing: '0.12em', textTransform: 'uppercase',
                    color: 'var(--accent)', border: '1px solid var(--accent)',
                    padding: '2px 8px', borderRadius: '2px', opacity: 0.85,
                  }}
                >
                  {tag}
                </span>
              ))}
            </div>

            <div className="flex flex-wrap gap-4 pt-2">
              <a
                href={campaign.discord}
                target="_blank" rel="noreferrer"
                className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                style={{ fontSize: '0.68rem', color: '#5865F2', textDecoration: 'none', letterSpacing: '0.1em' }}
              >
                <DiscordIcon />
                JOIN DISCORD
              </a>
              <a
                href="https://docs.google.com/presentation/d/1EAOe0iK-1s6i0UV5ObxSD86gGBj1Ixz6FOotQn5XPdc/edit#slide=id.g2b6a346170f_1_35"
                target="_blank" rel="noreferrer"
                className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                style={{ fontSize: '0.68rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
              >
                <ExternalLink size={11} />
                CAMPAIGN PRESENTATION
              </a>
            </div>
          </div>
        </section>

        {/* ── This campaign ── */}
        <section>
          <SectionHeading>THIS CAMPAIGN</SectionHeading>
          <div className="vs-card p-5 space-y-3">
            <div className="grid grid-cols-2 gap-4" style={{ fontSize: '0.78rem' }}>
              <InfoRow label="Campaign">{campaign.name}</InfoRow>
              <InfoRow label="Version">{campaign.version}</InfoRow>
              <InfoRow label="Server">{campaign.server}</InfoRow>
              {campaign.serverIp && (
                <InfoRow label="Server IP">
                  <span className="font-mono-vs" style={{ color: 'var(--accent)', fontSize: '0.72rem' }}>
                    {campaign.serverIp}
                  </span>
                </InfoRow>
              )}
              <InfoRow label="Blue Faction">{campaign.blueLabel}</InfoRow>
              <InfoRow label="Red Faction">{campaign.redLabel}</InfoRow>
            </div>
          </div>
        </section>

        {/* ── Engine Credits ── */}
        <section>
          <SectionHeading>ENGINE CREDITS</SectionHeading>

          <div className="space-y-3">
            {campaign.engineCredits.map((c) => {
              const roleColor = ROLE_COLORS[c.role] ?? 'var(--text-muted)'
              return (
                <div
                  key={c.name}
                  className="vs-card p-4 flex items-start gap-4"
                >
                  {/* Avatar monogram */}
                  <div
                    style={{
                      width: 40, height: 40, borderRadius: '3px', flexShrink: 0,
                      background: 'var(--bg-elevated)',
                      border: `1px solid ${roleColor}`,
                      display: 'flex', alignItems: 'center', justifyContent: 'center',
                      fontFamily: "'Bebas Neue', sans-serif",
                      fontSize: '1.1rem', letterSpacing: '0.06em', color: roleColor,
                    }}
                  >
                    {c.name.slice(0, 2).toUpperCase()}
                  </div>

                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 flex-wrap mb-1">
                      <span
                        className="font-display"
                        style={{ fontSize: '0.95rem', letterSpacing: '0.1em', color: 'var(--text)' }}
                      >
                        {c.name}
                      </span>
                      {c.handle && (
                        <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', letterSpacing: '0.05em' }}>
                          ({c.handle})
                        </span>
                      )}
                      {/* Role badge */}
                      <span
                        style={{
                          fontSize: '0.55rem', letterSpacing: '0.14em', textTransform: 'uppercase',
                          color: roleColor, border: `1px solid ${roleColor}`,
                          padding: '1px 7px', borderRadius: '2px',
                        }}
                      >
                        {c.role}
                      </span>
                    </div>
                  </div>
                </div>
              )
            })}
          </div>
        </section>

        {/* ── Server Credits (optional) ── */}
        {campaign.serverCredits.length > 0 && (
          <section>
            <SectionHeading>SERVER TEAM</SectionHeading>
            <div className="vs-card p-5">
              <div className="flex items-center gap-2 mb-3">
                <Users size={14} style={{ color: 'var(--accent)' }} />
                <span style={{ fontSize: '0.65rem', letterSpacing: '0.14em', textTransform: 'uppercase', color: 'var(--text-dim)' }}>
                  Operated By
                </span>
              </div>
              <div className="space-y-2">
                {campaign.serverCredits.map((s) => (
                  <div
                    key={s.name}
                    className="flex items-center justify-between"
                    style={{
                      padding: '0.5rem 0.75rem',
                      background: 'rgba(56,189,248,0.05)',
                      border: '1px solid rgba(56,189,248,0.12)',
                      borderRadius: '4px',
                    }}
                  >
                    <div className="flex items-center gap-3">
                      <div style={{ width: 6, height: 6, borderRadius: '50%', background: 'var(--accent)', flexShrink: 0 }} />
                      <span className="font-display" style={{ fontSize: '0.85rem', letterSpacing: '0.1em', color: 'var(--text)' }}>
                        {s.name}
                      </span>
                    </div>
                    <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', letterSpacing: '0.08em' }}>
                      {s.role}
                    </span>
                  </div>
                ))}
              </div>
            </div>
          </section>
        )}

        {/* ── Special Thanks ── */}
        <section>
          <SectionHeading>SPECIAL THANKS</SectionHeading>
          <div className="space-y-3">

            <div className="vs-card p-5">
              <div className="flex items-start gap-4">
                <img
                  src="https://www.digitalcombatsimulator.com/images/logos/Eagle_Dynamics.png"
                  alt="Eagle Dynamics"
                  style={{ width: 200, height: 'auto', flexShrink: 0, objectFit: 'contain' }}
                />
                <div>
                  <div className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)', marginBottom: '0.4rem' }}>
                    Eagle Dynamics
                  </div>
                  <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: '0.5rem' }}>
                    Developers of DCS World — the premier military flight simulator that
                    Fowl Engine is built upon.
                  </p>
                  <a
                    href="https://www.digitalcombatsimulator.com"
                    target="_blank" rel="noreferrer"
                    className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                    style={{ fontSize: '0.65rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
                  >
                    <ExternalLink size={10} />
                    DIGITALCOMBATSIMULATOR.COM
                  </a>
                </div>
              </div>
            </div>

            <div className="vs-card p-5">
              <div className="flex items-start gap-4">
                <img
                  src="/hacker.jpg"
                  alt="Hacker"
                  style={{ width: 48, height: 48, borderRadius: '8px', flexShrink: 0, objectFit: 'cover' }}
                />
                <div>
                  <div className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)', marginBottom: '0.4rem' }}>
                    61st ATP
                  </div>
                  <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
                    A persistent, 24/7 campaign lives or dies on having a machine that never goes offline —
                    and Vector Strike has one because <strong style={{ color: 'var(--text)' }}>Hacker</strong>,
                    founder of <strong style={{ color: 'var(--text)' }}>61st ATP — Arctic Terns &amp; Penguins</strong>,
                    generously donated the server it runs on. Every objective captured, every sortie flown, and
                    every name on the leaderboard is only possible because of that support. On behalf of the
                    whole squadron: thank you.
                  </p>
                </div>
              </div>
            </div>

            <div className="vs-card p-5">
              <div className="flex items-start gap-4">
                <img
                  src="/special-k.png"
                  alt="Special K"
                  style={{ width: 48, height: 48, borderRadius: '8px', flexShrink: 0, objectFit: 'contain' }}
                />
                <div>
                  <div className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)', marginBottom: '0.4rem' }}>
                    DCSServerBot
                  </div>
                  <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: '0.5rem' }}>
                    Built by <strong style={{ color: 'var(--text)' }}>Special K</strong>, DCSServerBot is the
                    Discord framework running our server bot — mission control, player management, and the live
                    campaign integration that ties Vector Strike's Discord presence to the fight in the air.
                  </p>
                  <a
                    href="https://github.com/Special-K-s-Flightsim-Bots/DCSServerBot"
                    target="_blank" rel="noreferrer"
                    className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                    style={{ fontSize: '0.65rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
                  >
                    <ExternalLink size={10} />
                    GITHUB.COM/DCSSERVERBOT
                  </a>
                </div>
              </div>
            </div>

          </div>
        </section>

        {/* ── Server info footer ── */}
        {campaign.serverIp && (
          <div
            className="flex items-center gap-2"
            style={{ fontSize: '0.68rem', color: 'var(--text-dim)', letterSpacing: '0.1em' }}
          >
            <Server size={11} />
            <span>DCS SERVER</span>
            <span className="font-mono-vs" style={{ color: 'var(--text-muted)' }}>{campaign.serverIp}</span>
          </div>
        )}

      </div>
    </div>
  )
}

// ── Local helpers ─────────────────────────────────────────────────────────────

function SectionHeading({ children }: { children: React.ReactNode }) {
  return (
    <div
      className="mb-4 font-display"
      style={{
        fontSize: '1.1rem', letterSpacing: '0.18em',
        color: 'var(--text)',
        borderLeft: '3px solid var(--accent)',
        paddingLeft: '0.75rem',
      }}
    >
      {children}
    </div>
  )
}

function InfoRow({ label, children }: { label: string; children: React.ReactNode }) {
  return (
    <div>
      <div style={{ fontSize: '0.6rem', letterSpacing: '0.12em', textTransform: 'uppercase', color: 'var(--text-dim)', marginBottom: '0.2rem' }}>
        {label}
      </div>
      <div style={{ color: 'var(--text)', fontSize: '0.8rem' }}>{children}</div>
    </div>
  )
}

function DiscordIcon() {
  return (
    <svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
      <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
    </svg>
  )
}
