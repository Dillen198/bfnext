import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { Radio } from 'lucide-react'

export default function AlertBanner() {
  const { data: stats } = useQuery({
    queryKey: ['site-stats'],
    queryFn: api.stats,
    refetchInterval: 30_000,
  })

  const isLive = !!stats?.active_round

  return (
    <div
      style={{
        background: isLive ? 'rgba(77,124,15,0.08)' : 'rgba(30,30,30,0.5)',
        borderTop: `1px solid ${isLive ? 'rgba(77,124,15,0.25)' : 'rgba(40,40,40,0.8)'}`,
        borderBottom: `1px solid ${isLive ? 'rgba(77,124,15,0.25)' : 'rgba(40,40,40,0.8)'}`,
        padding: '0.6rem 1.5rem',
      }}
    >
      <div className="max-w-7xl mx-auto flex items-center justify-between gap-4 flex-wrap">
        <div className="flex items-center gap-3">
          {isLive ? (
            <>
              <span
                className="w-2 h-2 rounded-full bg-red-500 flex-shrink-0"
                style={{ animation: 'vs-pulse 1.5s ease-in-out infinite' }}
              />
              <span style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.8rem', letterSpacing: '0.2em', color: '#65a30d' }}>
                MISSION ACTIVE
              </span>
              <span style={{ color: 'rgba(255,255,255,0.15)', fontSize: '0.75rem' }}>·</span>
              <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontFamily: "'JetBrains Mono', monospace" }}>
                Round #{stats?.active_round?.id} · {stats?.active_round?.scenario}
              </span>
            </>
          ) : (
            <>
              <Radio size={12} style={{ color: 'var(--text-dim)' }} />
              <span style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.8rem', letterSpacing: '0.2em', color: 'var(--text-dim)' }}>
                STANDBY — NO ACTIVE MISSION
              </span>
            </>
          )}
        </div>

        {stats && (
          <div className="flex items-center gap-6">
            {[
              { label: 'Pilots', value: stats.total_pilots },
              { label: 'Objectives', value: stats.objective_count },
              { label: 'Total Kills', value: stats.total_kills },
            ].map(s => (
              <div key={s.label} className="flex items-center gap-2">
                <span style={{ fontFamily: "'JetBrains Mono', monospace", fontSize: '0.78rem', color: isLive ? '#65a30d' : 'var(--text-muted)', fontWeight: 700 }}>
                  {s.value.toLocaleString()}
                </span>
                <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.14em', textTransform: 'uppercase' }}>
                  {s.label}
                </span>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  )
}
