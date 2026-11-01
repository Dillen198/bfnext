import React from 'react'
import { useQuery, type UseQueryResult } from '@tanstack/react-query'
import QueryState from '../components/QueryState'
import { AirframeUsage, PointsEconomy } from '../components/CampaignStats'
import { useState, useMemo } from 'react'
import { useNavigate } from 'react-router-dom'
import { api, roundNo, type Pilot, type Squadron, type XpRow } from '../api'
import { useRound } from '../context/RoundContext'
import PageHeader from '../components/PageHeader'
import {
  Rankings,
  Award,
  Aircraft,
  ChevronUp,
  ChevronDown,
  Intercept,
  Cas,
  Logistics,
  RANK_ICON,
  type IconComponent,
} from '@icons'
import { rankFor, nextRankAt } from '../ranks'
import { pressable } from '@shared/web/a11y'
import { campaign } from '../config/campaign'

type SortKey = keyof Omit<Pilot, 'ucid' | 'name'> | 'kd' | 'score' | 'logistics'

// The figures are read off the board in one colour, amber phosphor (typed
// black on paper): the column being ranked on is lit, the rest are muted.
// Only losses carry a colour of their own.
const COLS: { key: SortKey; label: string; loss?: boolean; title?: string; width?: number }[] = [
  { key: 'air_kills',        label: 'A/A',     title: 'Air-to-air kills',    width: 52 },
  { key: 'ground_kills',     label: 'A/G',     title: 'Air-to-ground kills', width: 60 },
  { key: 'logistics',        label: 'Logi',    title: 'Logistics (repairs + supply transfers + troops + FARPs + deployments)', width: 56 },
  { key: 'kd',               label: 'K/D',     title: 'Kill / Death ratio', width: 52 },
  { key: 'deaths',           label: 'Deaths',  loss: true, title: 'Deaths', width: 56 },
  { key: 'captures',         label: 'Cap',     title: 'Objective captures', width: 44 },
  { key: 'hours',            label: 'Hours',   title: 'Flight hours',       width: 56 },
  { key: 'score',            label: 'Score',   title: 'Combined efficiency score', width: 60 },
  { key: 'repairs',          label: 'Rep',     title: 'Repairs',                               width: 40 },
  { key: 'supply_transfers', label: 'Sup',     title: 'Supply transfers',                      width: 40 },
  { key: 'deploys',          label: 'Dep',     title: 'Deployments',                           width: 40 },
  { key: 'donated_points',   label: 'Pts',     title: 'Points donated',     width: 44 },
]

function computeKd(p: Pilot): number {
  const k = p.air_kills + p.ground_kills
  return p.deaths > 0 ? k / p.deaths : k > 0 ? 999 : 0
}
function computeLogistics(p: Pilot): number {
  return p.repairs + p.supply_transfers + p.troops + p.farps + p.deploys
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
  if (col === 'kd')         return fmtKd(p)
  if (col === 'score')      return Math.round(computeScore(p)).toLocaleString()
  if (col === 'logistics')  return computeLogistics(p).toLocaleString()
  if (col === 'hours')      return `${p.hours.toFixed(1)}h`
  const v = p[col as keyof Pilot]
  return typeof v === 'number' ? v.toLocaleString() : String(v ?? 0)
}

// Brass, steel and bronze: the board's medal ribbons.
const MEDAL_COLOR = ['var(--brass)', 'var(--text-muted)', 'var(--orange)']

/** NATO steel blue / Soviet red for a pilot's side, else the dim body colour. */
function sideInk(side: string | null | undefined): string {
  return side === 'Blue' ? 'var(--blue)' : side === 'Red' ? 'var(--red)' : 'var(--text-dim)'
}

/** Podium badge for a 0-indexed placing: three stripes for first, one for third. */
function RankBadge({ rank, size }: { rank: number; size: number }) {
  const Icon = RANK_ICON[rank]
  if (!Icon) return null
  return <Icon size={size} strokeWidth={1.7} style={{ color: MEDAL_COLOR[rank], flexShrink: 0 }} />
}
const MEDAL_CLASS = ['rank-gold', 'rank-silver', 'rank-bronze']

// Four leaderboard views: Overall (score), Air-to-Air, Air-to-Ground, Logistics.
// Selecting a tab drives both the podium ranking and the table's default sort.
const TABS: { key: SortKey; label: string; icon: IconComponent }[] = [
  { key: 'score',       label: 'Overall',     icon: Rankings },
  { key: 'air_kills',   label: 'A/A',         icon: Intercept },
  { key: 'ground_kills',label: 'A/G',         icon: Cas },
  { key: 'logistics',   label: 'Logistics',   icon: Logistics },
]

function Seg<T extends string>({ name, value, options, onChange }: {
  name: string; value: T; options: readonly (readonly [T, string])[]; onChange: (v: T) => void
}) {
  return (
    <div role="group" aria-label={name} className="rsr-seg">
      {options.map(([v, label]) => (
        <button key={v} type="button" aria-pressed={value === v} onClick={() => onChange(v)}
          className={`rsr-key${v === 'Blue' ? ' rsr-key--blue' : v === 'Red' ? ' rsr-key--red' : ''}`}>
          {label}
        </button>
      ))}
    </div>
  )
}

const thS: React.CSSProperties = { padding: '10px 14px', textAlign: 'left', fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', fontWeight: 700, borderBottom: '1px solid var(--border)' }
const tdS: React.CSSProperties = { padding: '9px 14px', fontSize: '0.75rem', color: 'var(--text-muted)', borderBottom: '1px solid var(--border)' }

function SquadronTable({ q, onOpen }: { q: UseQueryResult<Squadron[]>; onOpen: (tag: string) => void }) {
  const rows = [...(q.data ?? [])].sort((a, b) => b.xp - a.xp)
  return (
    <div className="vs-card overflow-hidden">
      <table className="vs-table" style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>
          <th style={{ ...thS, width: 40 }}>#</th><th style={thS}>Squadron</th>
          <th style={{ ...thS, textAlign: 'right' }}>Members</th><th style={{ ...thS, textAlign: 'right' }}>XP</th>
          <th style={{ ...thS, textAlign: 'right' }}>XP / member</th>
        </tr></thead>
        <tbody>
          <QueryState colSpan={5} what="squadrons" isLoading={q.isLoading} isError={q.isError} error={q.error} onRetry={q.refetch}
            isEmpty={!q.isLoading && rows.length === 0} emptyText="No squadrons yet" />
          {rows.map((sq, i) => (
            <tr key={sq.tag} onClick={() => onOpen(sq.tag)} style={{ cursor: 'pointer' }} className="kill-row">
              <td style={{ ...tdS, color: 'var(--text-dim)' }}>{i + 1}</td>
              <td style={tdS}><span style={{ color: 'var(--accent-bright)', fontWeight: 700 }}>[{sq.tag}]</span> <span style={{ color: 'var(--text)' }}>{sq.name}</span></td>
              <td style={{ ...tdS, textAlign: 'right' }}>{sq.member_count}</td>
              <td style={{ ...tdS, textAlign: 'right' }}><span className="rsr-fig" style={{ fontSize: '1.15rem' }}>{sq.xp.toLocaleString()}</span></td>
              <td style={{ ...tdS, textAlign: 'right' }}>{sq.member_count ? Math.round(sq.xp / sq.member_count).toLocaleString() : '—'}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function XpTable({ q, onOpen }: { q: UseQueryResult<XpRow[]>; onOpen: (ucid: string) => void }) {
  const rows = q.data ?? []
  const max = Math.max(1, ...rows.map(r => r.xp))
  return (
    <div className="vs-card overflow-hidden">
      <table className="vs-table" style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>
          <th style={{ ...thS, width: 40 }}>#</th><th style={thS}>Pilot</th>
          <th style={{ ...thS, textAlign: 'right', width: 70 }}>Level</th><th style={{ ...thS, width: '40%' }}>XP</th>
        </tr></thead>
        <tbody>
          <QueryState colSpan={4} what="XP leaderboard" isLoading={q.isLoading} isError={q.isError} error={q.error} onRetry={q.refetch}
            isEmpty={!q.isLoading && rows.length === 0} emptyText="Nobody has earned XP yet" />
          {rows.map((r, i) => (
            <tr key={r.ucid} onClick={() => onOpen(r.ucid)} style={{ cursor: 'pointer' }} className="kill-row">
              <td style={{ ...tdS, color: 'var(--text-dim)' }}>{i + 1}</td>
              <td style={{ ...tdS, color: 'var(--text)', fontWeight: 600 }}>{r.name || r.ucid}</td>
              <td style={{ ...tdS, textAlign: 'right' }}>{r.level}</td>
              <td style={tdS}>
                <div className="flex items-center gap-2">
                  <div style={{ flex: 1, height: 8, background: 'var(--bg-elevated)', boxShadow: 'inset 0 0 0 1px var(--border)' }}>
                    <div style={{ width: `${(r.xp / max) * 100}%`, height: '100%', background: 'var(--accent)' }} />
                  </div>
                  <span className="rsr-fig" style={{ minWidth: 64, textAlign: 'right', fontSize: '1.15rem' }}>{r.xp.toLocaleString()}</span>
                </div>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

type Period = 'all' | 'round'
type SideF = 'All' | 'Blue' | 'Red'
type View = 'pilots' | 'squadrons' | 'xp'

export default function Leaderboard() {
  const navigate = useNavigate()
  const { selectedRound } = useRound()
  const { data: rounds = [] } = useQuery({ queryKey: ['rounds'], queryFn: () => api.rounds() })
  // "This round" is the round picked in the header's round selector, else
  // the one being played.
  const roundId = selectedRound ?? rounds.find(r => r.active)?.id ?? null
  const roundRow = rounds.find(r => r.id === roundId)
  const roundNum = roundRow ? roundNo(roundRow) : roundId
  const [period, setPeriod] = useState<Period>('all')
  const [sideF, setSideF] = useState<SideF>('All')
  const [view, setView] = useState<View>('pilots')
  const byRound = period === 'round' && roundId != null
  const { data: allPilots = [], isLoading, isError, error, refetch } = useQuery({
    queryKey: ['leaderboard', byRound ? roundId : 'all'],
    queryFn: () => (byRound ? api.leaderboardRound(roundId!) : api.leaderboard()),
    refetchInterval: 30_000,
  })
  // An older bfdb ignores ?round= and answers all-time; side comes only from
  // the all-time roster, so fill it in for round rows.
  const { data: allTime = [] } = useQuery({ queryKey: ['leaderboard', 'all'], queryFn: () => api.leaderboard(), enabled: byRound })
  const sideOf = useMemo(() => new Map(allTime.map(p => [p.ucid, p.side])), [allTime])
  const pilots = useMemo(() => allPilots
    .map(p => (p.side || !byRound ? p : { ...p, side: sideOf.get(p.ucid) ?? null }))
    .filter(p => sideF === 'All' || p.side === sideF), [allPilots, byRound, sideOf, sideF])
  const squadrons = useQuery({ queryKey: ['squadrons'], queryFn: () => api.squadrons(), enabled: view === 'squadrons' })
  const xp = useQuery({ queryKey: ['xp-top'], queryFn: () => api.xpTop(), enabled: view === 'xp' })
  const [tab,    setTab]    = useState<SortKey>('score')
  const [sort, setSort] = useState<SortKey>('score')
  const [asc,  setAsc]  = useState(false)
  const [search, setSearch] = useState('')

  function getSortVal(p: Pilot, key: SortKey): number {
    if (key === 'kd')         return computeKd(p)
    if (key === 'score')      return computeScore(p)
    if (key === 'logistics')  return computeLogistics(p)
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

  function selectTab(key: SortKey) {
    setView('pilots')
    setTab(key)
    setSort(key)
    setAsc(false)
  }

  const top3 = useMemo(() =>
    [...pilots].sort((a, b) => getSortVal(b, tab) - getSortVal(a, tab)).slice(0, 3),
    [pilots, tab]
  )

  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden' }}>
      <PageHeader art="hornet"
        icon={Rankings}
        title="HONOURS BOARD"
        stamp={<span className="stamp stamp--green">Cited</span>}
        sub={view === 'pilots' ? `${pilots.length} pilots${byRound ? ` · round ${roundNum}` : ' · all time'}` : view === 'squadrons' ? 'Squadrons by XP' : 'Pilots by XP'}
        right={
          <input type="text" placeholder="Search pilot…" value={search}
            onChange={e => setSearch(e.target.value)} className="vs-input lb-search"
            style={{ width: 180 }} />
        }
      />

      <div className="flex-1 overflow-auto vs-page">

        {/* ── Leaderboard tabs ── */}
        <div className="lb-tabs" style={{ display: 'flex', gap: 8, flexWrap: 'wrap' }}>
          {TABS.map(t => {
            const active = view === 'pilots' && tab === t.key
            return (
              <button key={t.key} type="button" className="vs-tab rsr-key" aria-pressed={active} onClick={() => selectTab(t.key)}
                style={{ padding: '6px 14px', gap: 7 }}>
                <t.icon size={13} strokeWidth={1.75} />
                {t.label}
              </button>
            )
          })}
          {([['squadrons', 'Squadrons', Award], ['xp', 'XP', Rankings]] as const).map(([v, label, Icon]) => {
            const active = view === v
            return (
              <button key={v} type="button" className="vs-tab rsr-key" aria-pressed={active} onClick={() => setView(v)}
                style={{ padding: '6px 14px', gap: 7 }}>
                <Icon size={13} strokeWidth={1.75} />{label}
              </button>
            )
          })}
          {view === 'pilots' && (
            <div className="flex items-center gap-2 ml-auto flex-wrap">
              <Seg name="Period" value={period} onChange={setPeriod}
                options={[['all', 'All time'], ['round', roundId != null ? `Round ${roundNum}` : 'This round']] as const} />
              <Seg name="Side" value={sideF} onChange={setSideF}
                options={[['All', 'Both sides'], ['Blue', campaign.blueLabel], ['Red', campaign.redLabel]] as const} />
            </div>
          )}
        </div>

        {view === 'squadrons' && <SquadronTable q={squadrons} onOpen={tag => navigate(`/squadrons?tag=${encodeURIComponent(tag)}`)} />}
        {view === 'xp' && <XpTable q={xp} onOpen={ucid => navigate(`/pilots?ucid=${encodeURIComponent(ucid)}`)} />}
        {view === 'pilots' && <>

        {/* ── Podium ── */}
        {top3.length >= 3 && (
          <div className="podium-grid" style={{ display: 'grid', gap: 12 }}>
            {top3.map((p, i) => {
              const score = Math.round(computeScore(p))
              const mc = MEDAL_COLOR[i]
              const prank = rankFor(score, p.side)
              const PRankIcon = prank.icon
              return (
                <div key={p.ucid}
                  {...pressable(() => navigate(`/pilots?ucid=${encodeURIComponent(p.ucid)}`))}
                  className="vs-card"
                  style={{
                    borderTop: `3px solid ${mc}`, padding: '16px 20px', cursor: 'pointer',
                    display: 'flex', flexDirection: 'column', gap: 10, position: 'relative',
                  }}
                >
                  <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
                    <RankBadge rank={i} size={26} />
                    <div>
                      <div style={{ fontSize: '0.65rem', fontWeight: 700, letterSpacing: '0.1em', color: mc }}
                        className={MEDAL_CLASS[i]}>
                        RANK #{i + 1}
                      </div>
                      <div style={{ fontSize: '1rem', fontWeight: 700, color: 'var(--text)', lineHeight: 1.2, marginTop: 2 }}>
                        {p.name}
                      </div>
                      {p.side && (
                        <div style={{ fontSize: '0.58rem', fontWeight: 700, letterSpacing: '0.16em', textTransform: 'uppercase', color: sideInk(p.side), marginTop: 2 }}>
                          {p.side === 'Blue' ? campaign.blueLabel : campaign.redLabel}
                        </div>
                      )}
                      <div style={{ display: 'flex', alignItems: 'center', gap: 5, marginTop: 4 }}>
                        <PRankIcon size={13} strokeWidth={1.5} style={{ color: 'var(--text-muted)', flexShrink: 0 }} />
                        <span style={{ fontSize: '0.6rem', color: 'var(--text-muted)', letterSpacing: '0.08em' }}>
                          {prank.title}
                        </span>
                      </div>
                    </div>
                  </div>
                  <div className="podium-stats" style={{ display: 'flex', gap: 16, flexWrap: 'wrap' }}>
                    {[
                      { label: 'A2A',   value: p.air_kills },
                      { label: 'A2G',   value: p.ground_kills },
                      { label: 'Logi',  value: computeLogistics(p) },
                      { label: 'K/D',   value: fmtKd(p) },
                      { label: 'Hours', value: `${p.hours.toFixed(1)}h` },
                      { label: 'Score', value: score, lit: true },
                    ].map(s => (
                      <div key={s.label}>
                        <div className={`rsr-fig${s.lit ? '' : ' rsr-fig--dim'}`} style={{ fontSize: s.lit ? '1.6rem' : '1.3rem' }}>
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
        <div className="vs-card" style={{
          padding: '10px 16px',
          display: 'flex', alignItems: 'center', gap: 8, flexWrap: 'wrap',
        }}>
          <Award size={13} style={{ color: 'var(--accent)', flexShrink: 0 }} />
          <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)', textTransform: 'uppercase', letterSpacing: '0.1em', fontWeight: 600 }}>Score:</span>
          <span className="font-mono-vs" style={{ fontSize: '0.65rem', color: 'var(--text-muted)' }}>
            Air×3 + Ground×2 + Cap×5 + FARP×4 + Deploy×2 + Repair + Supply + Troop×0.5 + Action − Death×2
          </span>
        </div>

        {/* ── Main table (scrolls internally, not the whole page) ── */}
        <div className="vs-card" style={{ overflow: 'hidden', display: 'flex', flexDirection: 'column', flex: 'none' }}>
          {/* Its own scroll, tall enough for a page of pilots: squeezed into
              what the podium left over it showed three rows. */}
          <div style={{ overflow: 'auto', maxHeight: 'max(440px, calc(100vh - 260px))' }}>
            <table className="vs-table" style={{ width: '100%', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ background: 'var(--bg-card-solid)', borderBottom: '1px solid var(--border)', position: 'sticky', top: 0, zIndex: 1 }}>
                  <th style={{ padding: '10px 14px', textAlign: 'left', width: 36, fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', fontWeight: 700 }}>#</th>
                  <th style={{ padding: '10px 14px', textAlign: 'left', fontSize: '0.65rem', color: 'var(--text-dim)', letterSpacing: '0.1em', textTransform: 'uppercase', fontWeight: 700 }}>Pilot</th>
                  {COLS.map(c => (
                    <th key={c.key} title={c.title} onClick={() => handleSort(c.key)}
                      style={{
                        padding: '10px 10px', textAlign: 'right', whiteSpace: 'nowrap',
                        width: c.width, fontSize: '0.62rem', fontWeight: 700,
                        letterSpacing: '0.1em', textTransform: 'uppercase',
                        cursor: 'pointer', userSelect: 'none', transition: 'color 0.12s',
                        color: sort === c.key ? 'var(--accent)' : 'var(--text-dim)',
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
                <QueryState
                  colSpan={COLS.length + 2} what="pilots" isLoading={isLoading}
                  isError={isError} error={error} onRetry={refetch}
                />
                {sorted.map((p, i) => {
                  const rank      = rankFor(computeScore(p), p.side)
                  const RankIcon  = rank.icon
                  const nextAt    = nextRankAt(computeScore(p))
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
                          <RankBadge rank={medalIdx} size={16} />
                        ) : (
                          <span className="font-mono-vs" style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>{i + 1}</span>
                        )}
                      </td>
                      <td style={{ padding: '9px 14px', minWidth: 180 }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
                          <div style={{
                            width: 30, height: 30, borderRadius: 2, flexShrink: 0,
                            background: 'var(--bg-elevated)',
                            border: `1px solid ${isTop3 ? MEDAL_COLOR[medalIdx] : 'var(--border)'}`,
                            display: 'flex', alignItems: 'center', justifyContent: 'center',
                            color: isTop3 ? MEDAL_COLOR[medalIdx] : sideInk(p.side),
                          }}
                          title={`${rank.title}${nextAt !== null ? ` — ${nextAt - Math.round(computeScore(p))} score to next rank` : ' — top of the ladder'}`}>
                            <RankIcon size={18} strokeWidth={1.5} />
                          </div>
                          <div>
                            <div style={{ fontSize: '0.82rem', fontWeight: 600, color: 'var(--text)', lineHeight: 1 }}>{p.name}</div>
                            <div style={{ fontSize: '0.58rem', color: 'var(--text-muted)', letterSpacing: '0.08em', marginTop: 3 }}>
                              {rank.title}
                            </div>
                            <div style={{ display: 'flex', alignItems: 'center', gap: 4, marginTop: 2 }}>
                              <Aircraft size={9} style={{ color: 'var(--text-dim)', opacity: 0.7 }} />
                              <span className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
                                {p.air_kills}a·{p.ground_kills}g · {p.hours.toFixed(1)}h
                              </span>
                            </div>
                          </div>
                        </div>
                      </td>
                      {COLS.map(c => (
                        <td key={c.key} style={{ padding: '9px 10px', textAlign: 'right' }}>
                          {sort === c.key ? (
                            <span className="rsr-fig" style={{ fontSize: '1.2rem' }}>{fmtVal(c.key, p)}</span>
                          ) : (
                            <span className="font-mono-vs" style={{
                              fontSize: '0.78rem', fontWeight: 600,
                              color: c.loss ? 'var(--red)' : 'var(--text-muted)',
                            }}>
                              {fmtVal(c.key, p)}
                            </span>
                          )}
                        </td>
                      ))}
                    </tr>
                  )
                })}
                <QueryState
                  colSpan={COLS.length + 2} what="pilots" isLoading={false}
                  isEmpty={!isLoading && !isError && sorted.length === 0}
                  emptyText={search.trim()
                    ? `No pilots match "${search.trim()}"`
                    : 'No pilots on the leaderboard yet'}
                />
              </tbody>
            </table>
          </div>
        </div>

        </>}

        {/* Campaign-wide readouts bfdb has always served and nothing showed:
            which airframes are actually being flown, and where the points
            economy sits per side. */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 items-start" style={{ marginTop: 16 }}>
          <div className="vs-card">
            <div className="lb-card-head">Airframe usage</div>
            <AirframeUsage />
          </div>
          <div className="vs-card">
            <div className="lb-card-head">Points economy</div>
            <PointsEconomy />
          </div>
        </div>

      </div>
    </div>
  )
}
