import { useQuery } from '@tanstack/react-query'
import { useState, useMemo } from 'react'
import { api, type Pilot } from '../api'
import PageHeader from '../components/PageHeader'
import { Award } from 'lucide-react'

type SortKey = keyof Omit<Pilot, 'ucid' | 'name'> | 'kd' | 'score'

const COLS: { key: SortKey; label: string; color?: string; title?: string }[] = [
  { key: 'air_kills',        label: 'Air',     color: 'text-blue-400',   title: 'Air kills' },
  { key: 'ground_kills',     label: 'Ground',  color: 'text-orange-400', title: 'Ground kills' },
  { key: 'kd',               label: 'K/D',     color: 'text-green-400',  title: 'Kill/death ratio' },
  { key: 'captures',         label: 'Cap',     color: 'text-yellow-400', title: 'Objective captures' },
  { key: 'score',            label: 'Score',   color: 'text-purple-400', title: 'Combined efficiency score' },
  { key: 'deaths',           label: 'Deaths',  color: 'text-red-400',    title: 'Deaths' },
  { key: 'repairs',          label: 'Repairs', title: 'Repairs performed' },
  { key: 'supply_transfers', label: 'Supply',  title: 'Supply transfers' },
  { key: 'troops',           label: 'Troops',  title: 'Troops transported' },
  { key: 'farps',            label: 'FARPs',   title: 'FARPs deployed' },
  { key: 'deploys',          label: 'Deploys', title: 'Unit deployments' },
  { key: 'actions',          label: 'Actions', title: 'Actions taken' },
  { key: 'donated_points',   label: 'Donated', color: 'text-cyan-400',   title: 'Points donated to coalition' },
  { key: 'hours',            label: 'Hours',   title: 'Flight hours' },
]

function computeKd(p: Pilot): number {
  const kills = p.air_kills + p.ground_kills
  return p.deaths > 0 ? kills / p.deaths : kills > 0 ? 999 : 0
}

function computeScore(p: Pilot): number {
  return (p.air_kills * 3) + (p.ground_kills * 2) + (p.captures * 5) +
    p.repairs + p.supply_transfers + (p.troops * 0.5) + (p.farps * 4) +
    (p.deploys * 2) + p.actions - (p.deaths * 2)
}

function fmtKd(p: Pilot): string {
  const kills = p.air_kills + p.ground_kills
  return p.deaths > 0 ? (kills / p.deaths).toFixed(2) : kills > 0 ? '∞' : '0.00'
}

function fmtVal(col: SortKey, p: Pilot): string {
  if (col === 'kd') return fmtKd(p)
  if (col === 'score') return Math.round(computeScore(p)).toString()
  if (col === 'hours') return `${p.hours.toFixed(1)}h`
  const v = p[col as keyof Pilot]
  return String(typeof v === 'number' ? v : 0)
}

const MEDALS = ['🥇', '🥈', '🥉']
const MEDAL_COLORS = ['rank-gold', 'rank-silver', 'rank-bronze']

export default function Leaderboard() {
  const { data: pilots = [], isLoading } = useQuery({
    queryKey: ['leaderboard'],
    queryFn: api.leaderboard,
    refetchInterval: 30_000,
  })
  const [sort, setSort] = useState<SortKey>('air_kills')
  const [asc, setAsc] = useState(false)
  const [search, setSearch] = useState('')

  function getSortValue(p: Pilot, key: SortKey): number {
    if (key === 'kd') return computeKd(p)
    if (key === 'score') return computeScore(p)
    const v = p[key as keyof Pilot]
    return typeof v === 'number' ? v : 0
  }

  const sorted = useMemo(() => {
    const filtered = pilots.filter(p => !search || p.name.toLowerCase().includes(search.toLowerCase()))
    return [...filtered].sort((a, b) => {
      const av = getSortValue(a, sort)
      const bv = getSortValue(b, sort)
      return asc ? av - bv : bv - av
    })
  }, [pilots, sort, asc, search])

  function handleSort(key: SortKey) {
    if (sort === key) setAsc(a => !a)
    else { setSort(key); setAsc(false) }
  }

  // Top 3 by combined score for highlight
  const top3Ucids = useMemo(() => {
    return [...pilots].sort((a, b) => computeScore(b) - computeScore(a)).slice(0, 3).map(p => p.ucid)
  }, [pilots])

  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader
        title="RANKINGS"
        sub={`${pilots.length} pilots registered · click column to sort`}
        right={
          <input
            type="text"
            placeholder="Search pilot…"
            value={search}
            onChange={e => setSearch(e.target.value)}
            className="vs-input w-44"
          />
        }
      />

      <div className="flex-1 overflow-auto p-4 space-y-4 grid-bg" style={{ background: 'var(--bg)' }}>
        {/* Top 3 podium */}
        {pilots.length >= 3 && (
          <div className="grid grid-cols-3 gap-4">
            {top3Ucids.slice(0, 3).map((ucid, i) => {
              const p = pilots.find(x => x.ucid === ucid)
              if (!p) return null
              const kills = p.air_kills + p.ground_kills
              const score = Math.round(computeScore(p))
              const colors = ['#fbbf24', '#94a3b8', '#d97706']
              return (
                <div
                  key={ucid}
                  className="vs-card p-4 flex flex-col items-center text-center"
                  style={{ borderColor: `${colors[i]}22` } as React.CSSProperties}
                >
                  <div className="absolute inset-x-0 top-0 h-px" style={{ background: `linear-gradient(to right,transparent,${colors[i]}55,transparent)` }} />
                  <div className="text-2xl mb-1">{MEDALS[i]}</div>
                  <div className={`text-[12px] font-bold mb-0.5 ${MEDAL_COLORS[i]}`}>#{i + 1}</div>
                  <div className="text-[14px] font-bold text-slate-100 mb-1">{p.name}</div>
                  <div className="flex gap-4 text-[11px]">
                    <div>
                      <div className="font-mono font-bold text-blue-400">{kills}</div>
                      <div className="text-slate-700">kills</div>
                    </div>
                    <div>
                      <div className="font-mono font-bold text-green-400">{fmtKd(p)}</div>
                      <div className="text-slate-700">K/D</div>
                    </div>
                    <div>
                      <div className="font-mono font-bold text-purple-400">{score}</div>
                      <div className="text-slate-700">score</div>
                    </div>
                  </div>
                </div>
              )
            })}
          </div>
        )}

        {/* Score legend */}
        <div className="vs-card px-5 py-3 flex items-center gap-2 flex-wrap">
          <Award size={12} className="text-purple-400 flex-shrink-0" />
          <span className="text-[11px] text-slate-600 uppercase tracking-widest">Score formula:</span>
          <span className="text-[11px] text-slate-500 font-mono">
            Air×3 + Ground×2 + Capture×5 + FARP×4 + Deploy×2 + Repair + Supply + Troop×0.5 + Action − Death×2
          </span>
        </div>

        {/* Main table */}
        <div className="vs-card overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead className="border-b border-[#2a2a2a] sticky top-0 bg-[#191919] z-10">
                <tr>
                  <th className="px-4 py-3 text-left text-[11px] uppercase tracking-widest text-slate-700 w-8">#</th>
                  <th className="px-4 py-3 text-left text-[11px] uppercase tracking-widest text-slate-700">Pilot</th>
                  {COLS.map(c => (
                    <th
                      key={c.key}
                      title={c.title}
                      onClick={() => handleSort(c.key)}
                      className={`px-3 py-3 text-right text-[11px] uppercase tracking-widest cursor-pointer select-none whitespace-nowrap transition-colors ${
                        sort === c.key ? (c.color ?? 'text-blue-400') : 'text-slate-700 hover:text-slate-500'
                      }`}
                    >
                      {c.label}{sort === c.key ? (asc ? ' ↑' : ' ↓') : ''}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {isLoading && (
                  <tr><td colSpan={COLS.length + 2} className="text-center py-10 text-slate-700 text-sm">Loading…</td></tr>
                )}
                {sorted.map((p, i) => {
                  const medalIdx = top3Ucids.indexOf(p.ucid)
                  const isTop3 = medalIdx >= 0
                  const totalKills = p.air_kills + p.ground_kills

                  return (
                    <tr
                      key={p.ucid}
                      className={`border-b border-[#2a2a2a] kill-row transition-colors group ${
                        isTop3 ? 'bg-white/[0.01]' : ''
                      }`}
                    >
                      <td className="px-4 py-3">
                        {isTop3 ? (
                          <span className={`text-base ${MEDAL_COLORS[medalIdx]}`}>{MEDALS[medalIdx]}</span>
                        ) : (
                          <span className="text-[12px] text-slate-700 font-mono">{i + 1}</span>
                        )}
                      </td>
                      <td className="px-4 py-3">
                        <div className="flex items-center gap-2.5">
                          <div className={`w-7 h-7 rounded flex items-center justify-center text-[12px] font-bold flex-shrink-0 ${
                            isTop3 ? 'bg-[#4d7c0f15] text-green-300 border border-[#4d7c0f25]' : 'bg-[#191919] text-slate-500 border border-[#2a2a2a]'
                          }`}>
                            {p.name[0]?.toUpperCase()}
                          </div>
                          <div>
                            <div className="text-[13px] font-semibold text-slate-100">{p.name}</div>
                            <div className="text-[10px] text-slate-700 font-mono">{totalKills} kills · {p.hours.toFixed(1)}h</div>
                          </div>
                        </div>
                      </td>
                      {COLS.map(c => (
                        <td
                          key={c.key}
                          className={`px-3 py-3 text-right text-[12px] font-mono tabular-nums ${
                            sort === c.key
                              ? (c.color ?? 'text-blue-400')
                              : `${c.color ?? 'text-slate-500'} opacity-60 group-hover:opacity-100`
                          }`}
                        >
                          {fmtVal(c.key, p)}
                        </td>
                      ))}
                    </tr>
                  )
                })}
                {!isLoading && sorted.length === 0 && (
                  <tr><td colSpan={COLS.length + 2} className="text-center py-10 text-slate-700 text-sm">No pilots found</td></tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  )
}
