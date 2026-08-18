import { useQuery } from '@tanstack/react-query'
import { useState, useMemo } from 'react'
import { useNavigate } from 'react-router-dom'
import { api, type Pilot } from '../api'
import PageHeader from '../components/PageHeader'
import { Trophy, Plane, ChevronUp, ChevronDown } from 'lucide-react'

type SortKey = keyof Omit<Pilot, 'ucid' | 'name'> | 'kd' | 'score'

const COLS: { key: SortKey; label: string; color?: string; title?: string; width?: number }[] = [
  { key: 'air_kills',        label: 'Air',     color: '#60a5fa',  title: 'Air kills',          width: 52 },
  { key: 'ground_kills',     label: 'Ground',  color: '#fb923c',  title: 'Ground kills',       width: 60 },
  { key: 'kd',               label: 'K/D',     color: '#4ade80',  title: 'Kill / Death ratio', width: 52 },
  { key: 'deaths',           label: 'Deaths',  color: '#f87171',  title: 'Deaths',             width: 56 },
  { key: 'captures',         label: 'Cap',     color: '#fbbf24',  title: 'Objective captures', width: 44 },
  { key: 'hours',            label: 'Hours',   color: '#a78bfa',  title: 'Flight hours',       width: 56 },
  { key: 'score',            label: 'Score',   color: '#22d3ee',  title: 'Combined efficiency score', width: 60 },
  { key: 'repairs',          label: 'Rep',     title: 'Repairs',                               width: 40 },
  { key: 'supply_transfers', label: 'Sup',     title: 'Supply transfers',                      width: 40 },
  { key: 'deploys',          label: 'Dep',     title: 'Deployments',                           width: 40 },
  { key: 'donated_points',   label: 'Pts',     color: '#22d3ee',  title: 'Points donated',     width: 44 },
]

function computeKd(p: Pilot): number {
  const k = p.air_kills + p.ground_kills
  return p.deaths > 0 ? k / p.deaths : k > 0 ? 999 : 0
}
function computeScore(p: Pilot): number {
  return (p.air_kills * 3) + (p.ground_kills * 2) + (p.captures * 5) +
    p.repairs + p.supply_transfers + (p.troops * 0.5) + (p.farps * 4) +
    (p.deploys * 2) + p.actions - (p.deaths * 2)
}
function fmtKd(p: Pilot): string {
  const k = p.air_kills + p.ground_kills
  return p.deaths > 0 ? (k / p.deaths).toFixed(2) : k > 0 ? '∞' : '0.00'
}
function fmtVal(col: SortKey, p: Pilot): string {
  if (col === 'kd')    return fmtKd(p)
  if (col === 'score') return Math.round(computeScore(p)).toLocaleString()
  if (col === 'hours') return `${p.hours.toFixed(1)}h`
  const v = p[col as keyof Pilot]
  return typeof v === 'number' ? v.toLocaleString() : String(v ?? 0)
}

const MEDAL_ICON  = ['🥇', '🥈', '🥉']
const MEDAL_COLOR = ['#fbbf24', '#94a3b8', '#d97706']
const MEDAL_CLASS = ['rank-gold', 'rank-silver', 'rank-bronze']

export default function Leaderboard() {
  const navigate = useNavigate()
  const { data: pilots = [], isLoading } = useQuery({
    queryKey: ['leaderboard'],
    queryFn: api.leaderboard,
    refetchInterval: 30_000,
  })
  const [sort, setSort] = useState<SortKey>('score')
  const [asc,  setAsc]  = useState(false)
  const [search, setSearch] = useState('')

  function getSortVal(p: Pilot, key: SortKey): number {
    if (key === 'kd')    return computeKd(p)
    if (key === 'score') return computeScore(p)
    const v = p[key as keyof Pilot]
    return typeof v === 'number' ? v : 0
  }

  const sorted = useMemo(() => {
    const filtered = pilots.filter(p => !search || p.name.toLowerCase().includes(search.toLowerCase()))
    return [...filtered].sort((a, b) => {
      const d = getSortVal(a, sort) - getSortVal(b, sort)
      return asc ? d : -d
    })
  }, [pilots, sort, asc, search])

  function handleSort(key: SortKey) {
    if (sort === key) setAsc(a => !a)
    else { setSort(key); setAsc(false) }
  }

  const top3 = useMemo(() =>
    [...pilots].sort((a, b) => computeScore(b) - computeScore(a)).slice(0, 3),
    [pilots]
  )

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden' }}>
      <PageHeader
        title="RANKINGS"
        sub={`${pilots.length} pilots registered`}
        right={
          <input type="text" placeholder="Search pilot…" value={search}
            onChange={e => setSearch(e.target.value)} className="vs-input"
            style={{ width: 180 }} />
        }
      />

      <div className="flex-1 overflow-auto vs-page">

        {/* ── Podium ── */}
        {top3.length >= 3 && (
          <div className="podium-grid" style={{ display: 'grid', gap: 12 }}>
            {top3.map((p, i) => {
              const score = Math.round(computeScore(p))
              const mc = MEDAL_COLOR[i]
              return (
                <div key={p.ucid}
                  onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)}
                  style={{
                    background: 'var(--bg-card)', border: `1px solid ${mc}22`,
                    borderTop: `3px solid ${mc}`, borderRadius: 8, padding: '16px 20px',
                    cursor: 'pointer', transition: 'box-shadow 0.15s',
                    display: 'flex', flexDirection: 'column', gap: 8, position: 'relative',
                  }}
                  onMouseEnter={e => (e.currentTarget.style.boxShadow = `0 4px 24px ${mc}18`)}
                  onMouseLeave={e => (e.currentTarget.style.boxShadow = 'none')}
                >
                  <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
                    <span style={{ fontSize: '1.6rem', lineHeight: 1 }}>{MEDAL_ICON[i]}</span>
                    <div>
                      <div style={{ fontSize: '0.65rem', fontWeight: 700, letterSpacing: '0.1em', color: mc }}
                        className={MEDAL_CLASS[i]}>
                        RANK #{i + 1}
                      </div>
                      <div style={{ fontSize: '1rem', fontWeight: 700, color: 'var(--text)', lineHeight: 1.2, marginTop: 2 }}>
                        {p.name}
                      </div>
                    </div>
                  </div>
                  <div className="podium-stats" style={{ display: 'flex', gap: 16, flexWrap: 'wrap' }}>
                    {[
                      { label: 'A2A',   value: p.air_kills,              color: '#60a5fa' },
                      { label: 'A2G',   value: p.ground_kills,           color: '#fb923c' },
                      { label: 'K/D',   value: fmtKd(p),                 color: '#4ade80' },
                      { label: 'Hours', value: `${p.hours.toFixed(1)}h`, color: '#a78bfa' },
                      { label: 'Score', value: score,                    color: '#22d3ee' },
                    ].map(s => (
                      <div key={s.label}>
                        <div className="font-mono-vs" style={{ fontSize: '0.95rem', fontWeight: 700, color: s.color, lineHeight: 1 }}>
                          {s.value}
                        </div>
                        <div style={{ fontSize: '0.58rem', color: 'var(--text-dim)', marginTop: 2, textTransform: 'uppercase', letterSpacing: '0.1em' }}>
                          {s.label}
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )
            })}
          </div>
        )}

        {/* ── Score formula ── */}
        <div style={{
          background: 'var(--bg-card)', border: '1px solid var(--border)',
          borderRadius: 6, padding: '10px 16px',
          display: 'flex', alignItems: 'center', gap: 8, flexWrap: 'wrap',
        }}>
          <Trophy size={13} style={{ color: '#22d3ee', flexShrink: 0 }} />
          <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.1em', fontWeight: 600 }}>Score:</span>
          <span className="font-mono-vs" style={{ fontSize: '0.65rem', color: 'var(--text-muted)' }}>
            Air×3 + Ground×2 + Cap×5 + FARP×4 + Deploy×2 + Repair + Supply + Troop×0.5 + Action − Death×2
          </span>
        </div>

        {/* ── Main table ── */}
        <div style={{ background: 'var(--bg-card)', border: '1px solid var(--border)', borderRadius: 8, overflow: 'hidden', flexShrink: 0 }}>
          <div style={{ overflowX: 'auto' }}>
            <table style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'rgba(0,0,0,0.25)', borderBottom: '1px solid var(--border)' }}>
                  <th style={{ padding: '10px 14px', textAlign: 'left', width: 36, fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', fontWeight: 700 }}>#</th>
                  <th style={{ padding: '10px 14px', textAlign: 'left', fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', fontWeight: 700 }}>Pilot</th>
                  {COLS.map(c => (
                    <th key={c.key} title={c.title} onClick={() => handleSort(c.key)}
                      style={{
                        padding: '10px 10px', textAlign: 'right', whiteSpace: 'nowrap',
                        width: c.width, fontSize: '0.62rem', fontWeight: 700,
                        letterSpacing: '0.1em', textTransform: 'uppercase',
                        cursor: 'pointer', userSelect: 'none', transition: 'color 0.12s',
                        color: sort === c.key ? (c.color ?? 'var(--accent)') : 'var(--text-dim)',
                      }}
                    >
                      <span style={{ display: 'inline-flex', alignItems: 'center', gap: 3 }}>
                        {c.label}
                        {sort === c.key
                          ? asc ? <ChevronUp size={10} /> : <ChevronDown size={10} />
                          : <ChevronDown size={10} style={{ opacity: 0.2 }} />}
                      </span>
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {isLoading && (
                  <tr><td colSpan={COLS.length + 2} style={{ textAlign: 'center', padding: '3rem', color: 'var(--text-dim)', fontSize: '0.8rem' }}>Loading…</td></tr>
                )}
                {sorted.map((p, i) => {
                  const medalIdx = top3.findIndex(x => x.ucid === p.ucid)
                  const isTop3   = medalIdx >= 0

                  return (
                    <tr key={p.ucid}
                      onClick={() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`)}
                      style={{ borderBottom: '1px solid var(--border)', cursor: 'pointer', transition: 'background 0.1s' }}
                      className="kill-row"
                    >
                      <td style={{ padding: '9px 14px', width: 36 }}>
                        {isTop3 ? (
                          <span style={{ fontSize: '1rem' }}>{MEDAL_ICON[medalIdx]}</span>
                        ) : (
                          <span className="font-mono-vs" style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>{i + 1}</span>
                        )}
                      </td>
                      <td style={{ padding: '9px 14px', minWidth: 180 }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
                          <div style={{
                            width: 30, height: 30, borderRadius: 6, flexShrink: 0,
                            background: isTop3 ? `${MEDAL_COLOR[medalIdx]}18` : 'var(--bg-elevated)',
                            border: `1px solid ${isTop3 ? MEDAL_COLOR[medalIdx] + '33' : 'var(--border)'}`,
                            display: 'flex', alignItems: 'center', justifyContent: 'center',
                            fontSize: '0.75rem', fontWeight: 700,
                            color: isTop3 ? MEDAL_COLOR[medalIdx] : 'var(--text-dim)',
                          }}>
                            {p.name[0]?.toUpperCase()}
                          </div>
                          <div>
                            <div style={{ fontSize: '0.82rem', fontWeight: 600, color: 'var(--text)', lineHeight: 1 }}>{p.name}</div>
                            <div style={{ display: 'flex', alignItems: 'center', gap: 4, marginTop: 2 }}>
                              <Plane size={9} style={{ color: 'var(--text-dim)', opacity: 0.7 }} />
                              <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
                                {p.air_kills}a·{p.ground_kills}g · {p.hours.toFixed(1)}h
                              </span>
                            </div>
                          </div>
                        </div>
                      </td>
                      {COLS.map(c => (
                        <td key={c.key} style={{ padding: '9px 10px', textAlign: 'right' }}>
                          <span className="font-mono-vs" style={{
                            fontSize: '0.78rem', fontWeight: 600,
                            color: sort === c.key ? (c.color ?? 'var(--accent)') : (c.color ?? 'var(--text-muted)'),
                            opacity: sort === c.key ? 1 : 0.65,
                          }}>
                            {fmtVal(c.key, p)}
                          </span>
                        </td>
                      ))}
                    </tr>
                  )
                })}
                {!isLoading && sorted.length === 0 && (
                  <tr><td colSpan={COLS.length + 2} style={{ textAlign: 'center', padding: '3rem', color: 'var(--text-dim)', fontSize: '0.8rem' }}>No pilots found</td></tr>
                )}
              </tbody>
            </table>
          </div>
        </div>

      </div>
    </div>
  )
}
