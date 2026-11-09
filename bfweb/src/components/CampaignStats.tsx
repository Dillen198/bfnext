import { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { campaign } from '../config/campaign'
import { fmtHours } from '../lib/format'
import { useDashLook } from '../lib/look'
import QueryState from './QueryState'

/**
 * Two campaign-wide readouts bfdb has always served and nothing displayed:
 * `/api/aircraft-usage` (sorties and hours per airframe) and `/api/points`
 * (the pilot economy). Both are round-scoped and cheap -- a bounded sled
 * scan, no engine RPC -- so they poll slowly and follow the round selector.
 */

/** A bar's fill, never quite empty so the smallest row still shows one. */
const barPct = (frac: number) => `${Math.max(1, Math.round(frac * 100))}%`

function Bar({ frac, color }: { frac: number; color: string }) {
  return (
    <div style={{ height: 5, background: 'var(--bg-elevated)', flex: 1, minWidth: 40 }}>
      <div style={{ width: barPct(frac), height: '100%', background: color }} />
    </div>
  )
}

// The desert look (Levant Ablaze) draws both as charred readouts and rows of
// bars; its styles live in pages/levant/rankings.css with the Rankings page,
// the only page that shows them.
const sideTone = (s: string) => (s === 'Blue' ? 'b' : s === 'Red' ? 'r' : 'n')

function DBar({ frac, tone }: { frac: number; tone?: string }) {
  return <span className={`lb-cs-bar${tone ? ` ${tone}` : ''}`}><i style={{ width: barPct(frac) }} /></span>
}

export function AirframeUsage() {
  const look = useDashLook()
  const q = useQuery({
    queryKey: ['aircraft-usage'],
    queryFn: api.aircraftUsage,
    refetchInterval: 120_000,
  })
  const rows = useMemo(() => (q.data ?? []).slice(0, 12), [q.data])
  const max = rows.length ? Math.max(...rows.map(r => r.sorties)) : 1
  const totalSorties = (q.data ?? []).reduce((a, r) => a + r.sorties, 0)
  const totalHours = (q.data ?? []).reduce((a, r) => a + r.hours, 0)

  // Test the query, not the element: a JSX element is always truthy, so
  // `if (<QueryState/>)` would short-circuit even when it renders null.
  if (q.isLoading || q.isError || (q.data ?? []).length === 0) {
    return (
      <QueryState
        what="airframe usage" isLoading={q.isLoading} isError={q.isError}
        error={q.error} onRetry={q.refetch} isEmpty={(q.data ?? []).length === 0}
        emptyText="No sorties flown this round"
      />
    )
  }

  if (look.profile) {
    return (
      <div className="lb-cs">
        <dl className="lb-cs-sum">
          <div><dt>Sorties</dt><dd>{totalSorties.toLocaleString()}</dd></div>
          <div><dt>Airborne</dt><dd>{fmtHours(totalHours)}</dd></div>
          <div><dt>Types</dt><dd>{(q.data ?? []).length}</dd></div>
        </dl>
        <div className="lb-cs-head" aria-hidden="true"><span>Airframe</span><span /><span>Sorties</span><span>Hours</span></div>
        <ol className="lb-cs-list">
          {rows.map((r, i) => (
            <li key={r.vehicle} className={i === 0 ? 'first' : undefined}>
              <span className="nm" title={r.vehicle}>{r.vehicle}</span>
              <DBar frac={r.sorties / max} />
              <span className="v">{r.sorties.toLocaleString()}</span>
              <span className="h">{r.hours.toFixed(1)}h</span>
            </li>
          ))}
        </ol>
      </div>
    )
  }

  return (
    <div style={{ padding: '12px 16px 14px' }}>
      <div style={{ fontFamily: 'var(--font-mono)', fontSize: '0.62rem', color: 'var(--text-dim)', marginBottom: 9 }}>
        {totalSorties.toLocaleString()} sorties · {fmtHours(totalHours)} airborne · {(q.data ?? []).length} types
      </div>
      {rows.map(r => (
        <div key={r.vehicle} style={{ display: 'flex', alignItems: 'center', gap: 9, padding: '3px 0', fontSize: '0.68rem' }}>
          <span style={{ width: 132, flexShrink: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
            {r.vehicle}
          </span>
          <Bar frac={r.sorties / max} color="var(--accent)" />
          <span className="font-mono-vs" style={{ width: 42, textAlign: 'right', flexShrink: 0, color: 'var(--text-muted)' }}>
            {r.sorties}
          </span>
          <span className="font-mono-vs" style={{ width: 54, textAlign: 'right', flexShrink: 0, color: 'var(--text-dim)', fontSize: '0.62rem' }}>
            {r.hours.toFixed(1)}h
          </span>
        </div>
      ))}
    </div>
  )
}

export function PointsEconomy() {
  const look = useDashLook()
  const q = useQuery({
    queryKey: ['points'],
    queryFn: api.points,
    refetchInterval: 120_000,
  })

  const { blue, red, top } = useMemo(() => {
    const d = q.data ?? []
    return {
      blue: d.filter(p => p.side === 'Blue').reduce((a, p) => a + p.points, 0),
      red: d.filter(p => p.side === 'Red').reduce((a, p) => a + p.points, 0),
      top: [...d].sort((a, b) => b.points - a.points).slice(0, 8),
    }
  }, [q.data])

  if (q.isLoading || q.isError || (q.data ?? []).length === 0) {
    return (
      <QueryState
        what="the points economy" isLoading={q.isLoading} isError={q.isError}
        error={q.error} onRetry={q.refetch} isEmpty={(q.data ?? []).length === 0}
        emptyText="No points banked this round"
      />
    )
  }

  const total = blue + red || 1
  const maxTop = top.length ? top[0].points : 1

  if (look.profile) {
    const share = (v: number) => Math.round((v / total) * 100)
    return (
      <div className="lb-cs">
        <div className="lb-pe-split">
          <div className="b"><span className="ds-k">{campaign.blueLabel}</span><span className="v">{blue.toLocaleString()}</span><span className="pct">{share(blue)}%</span></div>
          <div className="r"><span className="ds-k">{campaign.redLabel}</span><span className="v">{red.toLocaleString()}</span><span className="pct">{share(red)}%</span></div>
        </div>
        <div className="lb-pe-bar" role="img" aria-label={`${campaign.blueLabel} ${blue.toLocaleString()} points, ${campaign.redLabel} ${red.toLocaleString()} points`}>
          <i className="b" style={{ width: `${(blue / total) * 100}%` }} />
          <i className="r" style={{ width: `${(red / total) * 100}%` }} />
          <span className="mid" />
        </div>
        <div className="lb-cs-head pts" aria-hidden="true"><span>Top balances</span><span /><span>Points</span></div>
        <ol className="lb-cs-list pts">
          {top.map(p => (
            <li key={p.name} className={sideTone(p.side)}>
              <span className="nm" title={p.name}>{p.name}</span>
              <DBar frac={p.points / maxTop} tone={sideTone(p.side)} />
              <span className="v">{p.points.toLocaleString()}</span>
            </li>
          ))}
        </ol>
      </div>
    )
  }

  return (
    <div style={{ padding: '12px 16px 14px' }}>
      {/* Side split -- who can actually afford to act */}
      <div style={{ display: 'flex', justifyContent: 'space-between', fontFamily: 'var(--font-mono)', fontSize: '0.65rem', marginBottom: 4 }}>
        <span style={{ color: campaign.blueColor, fontWeight: 700 }}>{campaign.blueLabel} {blue.toLocaleString()}</span>
        <span style={{ color: campaign.redColor, fontWeight: 700 }}>{red.toLocaleString()} {campaign.redLabel}</span>
      </div>
      <div style={{ display: 'flex', height: 6, marginBottom: 12, background: 'var(--bg-elevated)' }}>
        <div style={{ width: `${(blue / total) * 100}%`, background: campaign.blueColor }} />
        <div style={{ width: `${(red / total) * 100}%`, background: campaign.redColor }} />
      </div>

      <div style={{
        fontFamily: 'var(--font-mono)', fontSize: '0.57rem', letterSpacing: '0.16em',
        textTransform: 'uppercase', color: 'var(--text-dim)', marginBottom: 5,
      }}>
        Top balances
      </div>
      {top.map(p => (
        <div key={p.name} style={{ display: 'flex', alignItems: 'center', gap: 9, padding: '3px 0', fontSize: '0.68rem' }}>
          <span style={{
            width: 7, height: 7, flexShrink: 0,
            background: p.side === 'Blue' ? campaign.blueColor : campaign.redColor,
          }} />
          <span style={{ width: 126, flexShrink: 0, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
            {p.name}
          </span>
          <Bar frac={p.points / maxTop} color={p.side === 'Blue' ? campaign.blueColor : campaign.redColor} />
          <span className="font-mono-vs" style={{ width: 58, textAlign: 'right', flexShrink: 0, color: 'var(--text-muted)' }}>
            {p.points.toLocaleString()}
          </span>
        </div>
      ))}
    </div>
  )
}
