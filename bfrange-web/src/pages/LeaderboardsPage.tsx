/** `/leaderboards` — who is best at what, over a time window. */
import { useState, type ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { DaysSeg } from '../components/Controls'
import { windowDays } from '../lib/days'
import { Empty, ErrorState, Loading } from '../components/States'
import { TONE_VAR, fmt, scoreTone } from '../lib/format'
import type { Leaderboards } from '../types'

type Col<T> = { h: string; n?: boolean; v: (r: T) => ReactNode }
type Board<K extends keyof Leaderboards> = {
  id: K
  label: string
  blurb: string
  cols: Col<Leaderboards[K][number]>[]
}

const scoreCell = (s: number | null | undefined, d = 2) =>
  <span style={{ color: TONE_VAR[scoreTone(s ?? null)] }}>{s === null || s === undefined ? '—' : fmt(s, d)}</span>

const BOARDS = [
  { id: 'bombing', label: 'Bombing CEP', blurb: 'Median miss distance (CEP) over every bomb. Lower is better; two drops minimum.',
    cols: [
      { h: 'CEP', n: true, v: r => <b>{r.cep_m === null ? '—' : `${fmt(r.cep_m, 1)} m`}</b> },
      { h: 'Avg score', n: true, v: r => scoreCell(r.avg_score) },
      { h: 'Bombs', n: true, v: r => r.count },
    ] } satisfies Board<'bombing'>,
  { id: 'strafe', label: 'Strafe accuracy', blurb: 'Hits per round fired on valid passes (foul-line passes do not count).',
    cols: [
      { h: 'Accuracy', n: true, v: r => <b>{r.avg_accuracy === null ? '—' : `${fmt(r.avg_accuracy, 1)}%`}</b> },
      { h: 'Passes', n: true, v: r => r.count },
    ] } satisfies Board<'strafe'>,
  { id: 'lso', label: 'LSO points', blurb: 'Average greenie-board points per graded pass (_OK_ 5 … cut 0).',
    cols: [
      { h: 'Avg points', n: true, v: r => <b>{scoreCell(r.avg_points)}</b> },
      { h: 'Traps', n: true, v: r => r.traps },
      { h: 'Graded passes', n: true, v: r => r.count ?? '—' },
    ] } satisfies Board<'lso'>,
  { id: 'aar', label: 'Air refuelling', blurb: 'Average AAR session score: join time, disconnects, stability in contact.',
    cols: [
      { h: 'Avg score', n: true, v: r => <b>{scoreCell(r.avg_score)}</b> },
      { h: 'Sessions', n: true, v: r => r.count },
    ] } satisfies Board<'aar'>,
  { id: 'duels', label: 'Duel ELO', blurb: 'Player-vs-player duels in the arenas, rated from 1500.',
    cols: [
      { h: 'ELO', n: true, v: r => <b>{fmt(r.elo)}</b> },
      { h: 'W–L', n: true, v: r => `${r.wins}–${r.losses}` },
    ] } satisfies Board<'duels'>,
  { id: 'missile_defense', label: 'Missile defence', blurb: 'Trainer shots defeated vs shots that would have killed you.',
    cols: [
      { h: 'Defeated', n: true, v: r => <b style={{ color: 'var(--datum)' }}>{r.defeated}</b> },
      { h: 'Killed', n: true, v: r => <span style={{ color: r.killed ? 'var(--wave)' : undefined }}>{r.killed}</span> },
      { h: 'Survival', n: true, v: r => (r.defeated + r.killed ? `${fmt((r.defeated / (r.defeated + r.killed)) * 100)}%` : '—') },
    ] } satisfies Board<'missile_defense'>,
] as const

type BoardId = (typeof BOARDS)[number]['id']

function Table<K extends keyof Leaderboards>({ rows, board }: { rows: Leaderboards[K]; board: Board<K> }) {
  if (!rows.length) return <Empty title="Nobody on this board yet">Fly some and you will be first.</Empty>
  return (
    <div className="table-scroll">
      <table className="table-range">
        <thead>
          <tr>
            <th style={{ width: 48 }}>#</th>
            <th>Pilot</th>
            {board.cols.map(c => <th key={c.h} className={c.n ? 'n' : ''}>{c.h}</th>)}
          </tr>
        </thead>
        <tbody>
          {rows.map((r, i) => (
            <tr key={r.ucid}>
              <td className="mono" style={{ color: i < 3 ? 'var(--ball)' : 'var(--dim)', fontWeight: i < 3 ? 700 : 400 }}>{i + 1}</td>
              <td><Link to={`/pilot/${encodeURIComponent(r.ucid)}`} className="font-semibold hover:underline">{r.name}</Link></td>
              {board.cols.map(c => <td key={c.h} className={c.n ? 'n' : ''}>{c.v(r as never)}</td>)}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

export default function LeaderboardsPage() {
  const [days, setDays] = useState(30)
  const [tab, setTab] = useState<BoardId>('bombing')
  const q = useQuery({ queryKey: ['leaderboards', days], queryFn: () => api.leaderboards(windowDays(days)) })
  const board = BOARDS.find(b => b.id === tab)!

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">Leaderboards</h1>
          <p className="sub m-0 mt-1">{board.blurb}</p>
        </div>
        <div className="actions"><DaysSeg value={days} onChange={setDays} options={[7, 30, 90, 0]} /></div>
      </div>
      <div className="tabs-range mb-3" role="tablist">
        {BOARDS.map(b => (
          <button key={b.id} role="tab" aria-selected={tab === b.id} onClick={() => setTab(b.id)}>{b.label}</button>
        ))}
      </div>
      <div className="panel">
        {q.isLoading ? (
          <Loading label="Tallying" />
        ) : q.error ? (
          <div className="panel-b"><ErrorState error={q.error} retry={() => q.refetch()} /></div>
        ) : q.data ? (
          <Table rows={q.data[board.id]} board={board as unknown as Board<typeof board.id>} />
        ) : null}
      </div>
      {days === 0 && <p className="text-[12px] dim mt-2">"All" covers the last 365 days, the longest window the server keeps aggregates for.</p>}
    </div>
  )
}
