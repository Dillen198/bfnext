import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { campaign } from '../config/campaign'
import { ArrowRight, Trophy } from 'lucide-react'
import Reveal from './Reveal'

function kd(air: number, ground: number, deaths: number): string {
  const k = air + ground
  return deaths > 0 ? (k / deaths).toFixed(2) : k > 0 ? '∞' : '0.00'
}

function score(p: { air_kills: number; ground_kills: number; deaths: number; hours: number }): number {
  return (p.air_kills * 3) + (p.ground_kills * 2) - (p.deaths * 2)
}

const MEDAL_COLORS = ['#fbbf24', '#94a3b8', '#d97706']

export default function StatsSection() {
  const { data: stats, isLoading: statsLoading } = useQuery({ queryKey: ['site-stats'],    queryFn: api.stats,       refetchInterval: 30_000 })
  const { data: objectives = [], isLoading: objectivesLoading } = useQuery({ queryKey: ['site-objectives'], queryFn: api.objectives, refetchInterval: 30_000 })
  const { data: pilots = [], isLoading: pilotsLoading }     = useQuery({ queryKey: ['site-pilots'],     queryFn: api.leaderboard, refetchInterval: 60_000 })

  const total       = objectives.length
  const blueCount   = objectives.filter(o => o.owner === 'Blue').length
  const redCount    = objectives.filter(o => o.owner === 'Red').length
  const neutralCount = objectives.filter(o => o.owner === 'Neutral').length
  const bluePct     = total > 0 ? (blueCount    / total) * 100 : 0
  const redPct      = total > 0 ? (redCount     / total) * 100 : 0
  const neutralPct  = total > 0 ? (neutralCount / total) * 100 : 0

  const top3 = [...pilots]
    .sort((a, b) => score(b) - score(a))
    .slice(0, 3)

  return (
    <section id="stats" style={{ background: 'var(--bg-alt)', padding: '6rem 0', borderTop: '1px solid var(--border)' }}>
      <div className="max-w-7xl mx-auto px-6">

        {/* Header */}
        <div className="flex items-end justify-between mb-12 flex-wrap gap-4">
          <div>
            <div className="vs-section-label mb-4">Live from the server</div>
            <h2
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: 'clamp(2.5rem, 6vw, 3.5rem)',
                color: 'var(--text)',
                letterSpacing: '0.08em',
                margin: 0,
              }}
            >
              CAMPAIGN <span style={{ color: 'var(--accent)' }}>STATUS</span>
            </h2>
          </div>
          <a
            href={campaign.dashboardUrl}
            className="vs-btn vs-btn-outline"
            style={{ fontSize: '0.85rem', padding: '0.55rem 1.25rem' }}
          >
            FULL DASHBOARD
            <ArrowRight size={14} />
          </a>
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">

          {/* Territory control */}
          <Reveal style={{ background: 'var(--bg-card)', border: '1px solid var(--border)', borderRadius: '2px', padding: '1.5rem' }}>
            <h3
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '0.9rem',
                letterSpacing: '0.2em',
                color: 'var(--text-muted)',
                margin: '0 0 1.25rem 0',
                display: 'flex',
                alignItems: 'center',
                gap: '0.5rem',
              }}
            >
              <span style={{ width: '3px', height: '1em', background: 'var(--accent)', borderRadius: '1px', display: 'inline-block' }} />
              TERRITORY CONTROL
            </h3>

            {objectivesLoading ? (
              <>
                {/* Loading bar — arrows advancing toward the front line */}
                <div className="territory-loading mb-3">
                  <div className="territory-loading-half territory-loading-blue" />
                  <div className="territory-loading-half territory-loading-red" />
                </div>
                <div className="text-center" style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.2em', textTransform: 'uppercase' }}>
                  Syncing front line…
                </div>
              </>
            ) : (
              <>
                {/* Bar */}
                <div className="territory-bar mb-3">
                  <div className="territory-bar-blue"  style={{ width: `${bluePct}%` }} />
                  <div className="territory-bar-neutral" style={{ width: `${neutralPct}%` }} />
                  <div className="territory-bar-red"   style={{ width: `${redPct}%` }} />
                </div>

                {/* Labels */}
                <div className="flex justify-between">
                  <div>
                    <div style={{ fontFamily: "'JetBrains Mono', monospace", fontSize: '1.2rem', color: '#3b82f6', fontWeight: 700 }}>
                      {Math.round(bluePct)}%
                    </div>
                    <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.2em', textTransform: 'uppercase' }}>
                      {campaign.blueLabel ?? 'BLUFOR'} · {blueCount} obj
                    </div>
                  </div>
                  <div className="text-center">
                    <div style={{ fontFamily: "'JetBrains Mono', monospace", fontSize: '1.2rem', color: 'var(--text-muted)', fontWeight: 700 }}>
                      {Math.round(neutralPct)}%
                    </div>
                    <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.2em', textTransform: 'uppercase' }}>
                      NEUTRAL · {neutralCount}
                    </div>
                  </div>
                  <div className="text-right">
                    <div style={{ fontFamily: "'JetBrains Mono', monospace", fontSize: '1.2rem', color: 'var(--accent)', fontWeight: 700 }}>
                      {Math.round(redPct)}%
                    </div>
                    <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.2em', textTransform: 'uppercase' }}>
                      {campaign.redLabel ?? 'REDFOR'} · {redCount} obj
                    </div>
                  </div>
                </div>
              </>
            )}

            {/* Summary stats */}
            <div className="vs-divider" style={{ margin: '1.25rem 0' }} />
            <div className="grid grid-cols-3 gap-4">
              {[
                { label: 'Total Objectives', value: stats?.objective_count ?? '—' },
                { label: 'Active Pilots',    value: stats?.total_pilots   ?? '—' },
                { label: 'Total Kills',      value: stats?.total_kills    ?? '—' },
              ].map(s => (
                <div key={s.label} className="text-center">
                  <div
                    className={statsLoading ? 'vs-pulse' : undefined}
                    style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.8rem', color: 'var(--text)', letterSpacing: '0.04em', lineHeight: 1 }}
                  >
                    {typeof s.value === 'number' ? s.value.toLocaleString() : s.value}
                  </div>
                  <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.18em', textTransform: 'uppercase', marginTop: '3px' }}>
                    {s.label}
                  </div>
                </div>
              ))}
            </div>
          </Reveal>

          {/* Top 3 pilots */}
          <Reveal delay={120} style={{ background: 'var(--bg-card)', border: '1px solid var(--border)', borderRadius: '2px', padding: '1.5rem' }}>
            <h3
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '0.9rem',
                letterSpacing: '0.2em',
                color: 'var(--text-muted)',
                margin: '0 0 1.25rem 0',
                display: 'flex',
                alignItems: 'center',
                gap: '0.5rem',
              }}
            >
              <span style={{ width: '3px', height: '1em', background: 'var(--accent)', borderRadius: '1px', display: 'inline-block' }} />
              TOP PILOTS
            </h3>

            {top3.length === 0 ? (
              <div
                className={pilotsLoading ? 'vs-pulse' : undefined}
                style={{ color: 'var(--text-dim)', fontSize: '0.85rem', textAlign: 'center', padding: '2rem 0' }}
              >
                {pilotsLoading ? 'Loading pilot data…' : 'Awaiting pilot data…'}
              </div>
            ) : (
              <div className="space-y-3">
                {top3.map((p, i) => (
                  <Reveal
                    key={p.ucid}
                    delay={i * 100}
                    className="flex items-center gap-4"
                    style={{
                      background: 'var(--bg-elevated)',
                      border: `1px solid ${i === 0 ? 'rgba(251,191,36,0.2)' : 'var(--border)'}`,
                      borderRadius: '2px',
                      padding: '0.85rem 1rem',
                    }}
                  >
                    {/* Rank number */}
                    <div
                      style={{
                        fontFamily: "'Bebas Neue', sans-serif",
                        fontSize: '1.6rem',
                        color: MEDAL_COLORS[i],
                        lineHeight: 1,
                        width: '2rem',
                        textAlign: 'center',
                        flexShrink: 0,
                      }}
                    >
                      #{i + 1}
                    </div>

                    {/* Avatar */}
                    <div
                      className="flex items-center justify-center w-8 h-8 flex-shrink-0"
                      style={{
                        background: `${MEDAL_COLORS[i]}15`,
                        border: `1px solid ${MEDAL_COLORS[i]}30`,
                        borderRadius: '2px',
                        fontFamily: "'Bebas Neue', sans-serif",
                        fontSize: '0.9rem',
                        color: MEDAL_COLORS[i],
                      }}
                    >
                      {p.name[0]?.toUpperCase()}
                    </div>

                    {/* Name + kills */}
                    <div className="flex-1 min-w-0">
                      <div style={{ fontSize: '0.9rem', fontWeight: 600, color: 'var(--text)', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                        {p.name}
                      </div>
                      <div style={{ fontFamily: "'JetBrains Mono', monospace", fontSize: '0.65rem', color: 'var(--text-muted)', marginTop: '1px' }}>
                        {p.air_kills + p.ground_kills} kills · {kd(p.air_kills, p.ground_kills, p.deaths)} K/D · {p.hours.toFixed(1)}h
                      </div>
                    </div>

                    {/* Score */}
                    <div className="text-right flex-shrink-0">
                      <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.4rem', color: MEDAL_COLORS[i], lineHeight: 1 }}>
                        {score(p)}
                      </div>
                      <div style={{ fontSize: '0.55rem', color: 'var(--text-dim)', letterSpacing: '0.15em', textTransform: 'uppercase' }}>
                        score
                      </div>
                    </div>

                    {i === 0 && <Trophy size={14} style={{ color: '#fbbf24', flexShrink: 0 }} />}
                  </Reveal>
                ))}
              </div>
            )}

            <div className="vs-divider" style={{ margin: '1.25rem 0' }} />
            <a
              href={`${campaign.dashboardUrl}/leaderboard`}
              style={{ fontSize: '0.75rem', color: 'var(--accent)', textDecoration: 'none', fontFamily: "'Bebas Neue', sans-serif", letterSpacing: '0.14em', display: 'flex', alignItems: 'center', gap: '0.4rem' }}
            >
              VIEW FULL LEADERBOARD <ArrowRight size={12} />
            </a>
          </Reveal>
        </div>
      </div>
    </section>
  )
}
