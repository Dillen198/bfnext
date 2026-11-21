/** `/leaderboards` — who is best at what, over a time window. */
import { useState, type ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { DaysSeg } from '../components/Controls'
import { windowDays } from '../lib/days'
import { Empty, ErrorState, Loading } from '../components/States'
import { TONE_VAR, fmt, fmtClock, scoreTone } from '../lib/format'
import type { Leaderboards } from '../types'

type Col<T> = { h: string; n?: boolean; v: (r: T) => ReactNode }
type Rows<K extends keyof Leaderboards> = NonNullable<Leaderboards[K]>
type Board<K extends keyof Leaderboards> = {
  id: K
  label: string
  blurb: string
  cols: Col<Rows<K>[number]>[]
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
  { id: 'sead', label: 'SEAD kills', blurb: 'IADS kills, ranked by the ones made while the radar was up (true suppression), then by total.',
    cols: [
      { h: 'Radar up', n: true, v: r => <b style={{ color: 'var(--datum)' }}>{r.radar_up}</b> },
      { h: 'Kills', n: true, v: r => r.count },
      { h: 'Sites destroyed', n: true, v: r => r.sites_destroyed },
    ] } satisfies Board<'sead'>,
  { id: 'hot_zone', label: 'Hot zone', blurb: 'Kills in the hot zones, air and ground. Deaths are trainer saves plus real shoot-downs.',
    cols: [
      { h: 'Kills', n: true, v: r => <b>{r.air_kills + r.ground_kills}</b> },
      { h: 'Air · ground', n: true, v: r => `${r.air_kills} · ${r.ground_kills}` },
      { h: 'Deaths', n: true, v: r => <span style={{ color: r.deaths ? 'var(--wave)' : undefined }}>{r.deaths}</span> },
      { h: 'Sorties', n: true, v: r => r.count },
    ] } satisfies Board<'hot_zone'>,
  { id: 'low_level', label: 'Low level', blurb: 'Best low-level route score, then the smallest time-on-target error on a run that made every gate.',
    cols: [
      { h: 'Best', n: true, v: r => <b>{scoreCell(r.best_score)}</b> },
      { h: 'Best TOT', n: true, v: r => (r.best_tot_s === null ? '—' : `±${fmt(r.best_tot_s, 1)} s`) },
      { h: 'Avg score', n: true, v: r => scoreCell(r.avg_score) },
      { h: 'Runs', n: true, v: r => r.count },
    ] } satisfies Board<'low_level'>,
  { id: 'field_landing', label: 'Landing grades', blurb: 'Average runway landing grade: aim point, centreline, sink rate and a stable approach. Three landings minimum.',
    cols: [
      { h: 'Avg score', n: true, v: r => <b>{scoreCell(r.avg_score)}</b> },
      { h: 'Aim error', n: true, v: r => (r.avg_aim_error_m === null ? '—' : `${fmt(r.avg_aim_error_m)} m`) },
      { h: 'Stable', n: true, v: r => `${fmt(r.stable_pct)}%` },
      { h: 'Landings', n: true, v: r => r.count },
    ] } satisfies Board<'field_landing'>,
  { id: 'deck_landing', label: 'Deck landings', blurb: 'Helicopter landings on ships under way. Two minimum.',
    cols: [
      { h: 'Avg score', n: true, v: r => <b>{scoreCell(r.avg_score)}</b> },
      { h: 'Off the spot', n: true, v: r => (r.avg_distance_m === null ? '—' : `${fmt(r.avg_distance_m, 1)} m`) },
      { h: 'Landings', n: true, v: r => r.count },
    ] } satisfies Board<'deck_landing'>,
  { id: 'csar', label: 'CSAR', blurb: 'Fastest rescue: from the MAYDAY to the survivor delivered home.',
    cols: [
      { h: 'Fastest', n: true, v: r => <b>{fmtClock(r.fastest_s)}</b> },
      { h: 'Rescues', n: true, v: r => r.rescues },
      { h: 'Attempts', n: true, v: r => r.count },
    ] } satisfies Board<'csar'>,
] as const

type BoardId = (typeof BOARDS)[number]['id']

function Table<K extends keyof Leaderboards>({ rows, board }: { rows: Rows<K>; board: Board<K> }) {
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
          <Table rows={q.data[board.id] ?? []} board={board as unknown as Board<typeof board.id>} />
        ) : null}
      </div>
      {days === 0 && <p className="text-[12px] dim mt-2">"All" covers the last 365 days, the longest window the server keeps aggregates for.</p>}
    </div>
  )
}
