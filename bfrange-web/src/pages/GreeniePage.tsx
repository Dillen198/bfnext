/**
 * `/greenie` — the ready-room board. One row per pilot, one square per pass
 * in the order flown, coloured by grade; the average counts only graded
 * passes (foul-deck waveoffs and no-counts are grey and do not count).
 */
import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { GreenieSquare } from '../components/Chips'
import { DaysSeg, Select } from '../components/Controls'
import { windowDays } from '../lib/days'
import { Empty, ErrorState, Loading } from '../components/States'
import { TONE_VAR, airframe, fmt, scoreTone } from '../lib/format'
import { GRADE_LEGEND, gradeName, gradeStyle } from '../lib/lso'

const CARRIER_TYPES = ['FA-18C_hornet', 'F-14B', 'F-14A-135-GR', 'T-45']

export default function GreeniePage() {
  const [days, setDays] = useState(30)
  const [carrier, setCarrier] = useState('')
  const [unitType, setUnitType] = useState('')
  const [night, setNight] = useState<'all' | 'day' | 'night'>('all')

  const q = useQuery({
    queryKey: ['greenie', days, carrier, unitType],
    queryFn: () => api.greenie({ days: windowDays(days), carrier: carrier || undefined, unit_type: unitType || undefined }),
  })
  // carrier names from the live picture, so the filter lists real decks
  const live = useQuery({ queryKey: ['live'], queryFn: () => api.live(), staleTime: 60_000 })
  const carriers = live.data?.live?.carriers.filter(c => /CVN|Stennis|Forrestal|Kuznetsov/i.test(c.name + c.unit_type)).map(c => c.name) ?? []

  const rows = useMemo(() => {
    const rs = (q.data?.rows ?? []).map(r => {
      const passes = r.passes.filter(p => night === 'all' || (night === 'night') === p.night)
      const pts = passes.map(p => p.points).filter((x): x is number => x !== null)
      return { ...r, passes, count: pts.length, avg_points: pts.length ? pts.reduce((a, b) => a + b, 0) / pts.length : null }
    }).filter(r => r.passes.length)
    return rs.sort((a, b) => (b.avg_points ?? -1) - (a.avg_points ?? -1) || b.count - a.count)
  }, [q.data, night])
  const maxPasses = Math.max(0, ...rows.map(r => r.passes.length))

  const airframes = useMemo(() => {
    const set = new Set(CARRIER_TYPES)
    q.data?.rows.forEach(r => r.passes.forEach(p => set.add(p.unit_type)))
    return [...set]
  }, [q.data])

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">Greenie board</h1>
          <p className="sub m-0 mt-1">Every graded carrier pass, in the order flown. Click a square for the LSO debrief and trap sheet.</p>
        </div>
        <div className="actions">
          <DaysSeg value={days} onChange={setDays} />
        </div>
      </div>

      <div className="flex flex-wrap items-end gap-3 mb-4">
        <Select
          label="Carrier"
          value={carrier}
          onChange={setCarrier}
          options={[{ value: '', label: 'All carriers' }, ...carriers.map(c => ({ value: c, label: c }))]}
        />
        <Select
          label="Airframe"
          value={unitType}
          onChange={setUnitType}
          options={[{ value: '', label: 'All airframes' }, ...airframes.map(a => ({ value: a, label: airframe(a) }))]}
        />
        <label className="field">
          <span>Day / night</span>
          <div className="seg">
            {(['all', 'day', 'night'] as const).map(n => (
              <button key={n} aria-pressed={night === n} onClick={() => setNight(n)}>{n === 'all' ? 'Both' : n[0].toUpperCase() + n.slice(1)}</button>
            ))}
          </div>
        </label>
        <div className="ml-auto flex flex-wrap gap-x-3 gap-y-1.5 items-center" aria-label="Legend">
          {GRADE_LEGEND.map(g => {
            const s = gradeStyle(g.grade)
            return (
              <span key={g.grade} className="inline-flex items-center gap-1.5 text-[12px]" title={gradeName(g.grade)}>
                <span className="inline-block w-3.5 h-3.5 rounded-[2px]" style={{ background: s.bg, border: g.grade === 'C' ? '1px solid var(--line-2)' : undefined }} />
                <span className="mono">{g.grade}</span>
                <span className="dim mono">{g.points}</span>
              </span>
            )
          })}
          <span className="inline-flex items-center gap-1.5 text-[12px] dim">
            <span className="relative inline-block w-3.5 h-3.5 rounded-[2px]" style={{ background: 'var(--g-ok)' }}>
              <span style={{ position: 'absolute', right: 0, top: 0, borderStyle: 'solid', borderWidth: '0 6px 6px 0', borderColor: 'transparent #000 transparent transparent' }} />
            </span>
            night
          </span>
        </div>
      </div>

      {q.isLoading ? (
        <Loading label="Chalking the board" />
      ) : q.error ? (
        <ErrorState error={q.error} retry={() => q.refetch()} />
      ) : !rows.length ? (
        <div className="panel"><Empty title="No passes in this window">Widen the time window or clear the filters.</Empty></div>
      ) : (
        <div className="panel table-scroll">
          <table className="table-range" style={{ minWidth: 520 }}>
            <thead>
              <tr>
                <th style={{ width: 36 }}>#</th>
                <th>Pilot</th>
                <th className="n">Avg</th>
                <th className="n">Graded</th>
                <th>Passes <span className="dim normal-case tracking-normal">· oldest → newest</span></th>
              </tr>
            </thead>
            <tbody>
              {rows.map((r, i) => (
                <tr key={r.ucid}>
                  <td className="mono dim">{i + 1}</td>
                  <td className="whitespace-nowrap">
                    <Link to={`/pilot/${encodeURIComponent(r.ucid)}`} className="font-semibold hover:underline">{r.name}</Link>
                  </td>
                  <td className="n">
                    <span className="text-[17px] font-semibold" style={{ color: TONE_VAR[scoreTone(r.avg_points)] }}>
                      {r.avg_points === null ? '—' : fmt(r.avg_points, 2)}
                    </span>
                  </td>
                  <td className="n muted">{r.count}</td>
                  <td>
                    <div className="flex gap-[3px] flex-wrap" style={{ minWidth: Math.min(maxPasses, 12) * 29 }}>
                      {r.passes.map(p => <GreenieSquare key={p.id} pass={p} />)}
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  )
}
