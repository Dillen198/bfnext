/**
 * `/stations/:id` — every impact on one target on a single diagram: the
 * group's CEP (the radius holding half the impacts) and its mean point of
 * impact, which is where a systematic aiming error shows up.
 */
import { useMemo, useState } from 'react'
import { Link, useParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api'
import { DaysSeg, Panel, Select } from '../components/Controls'
import { windowDays } from '../lib/days'
import { QualityChip } from '../components/Chips'
import { BombPlot } from '../components/plots/BombPlot'
import { Empty, ErrorState, Loading } from '../components/States'
import { TONE_VAR, fmt, fmtDateTime, mToFt, qualityTone, weaponName } from '../lib/format'
import { bombBands, SCORING_DEFAULTS } from '../lib/grading'
import type { BombQuality } from '../types'

const QUALITIES: BombQuality[] = ['SHACK', 'EXCELLENT', 'GOOD', 'INEFFECTIVE', 'POOR']

export default function StationPage() {
  const { id = '' } = useParams()
  const [days, setDays] = useState(30)
  const [pilot, setPilot] = useState('')
  const [weapon, setWeapon] = useState('')

  const q = useQuery({
    queryKey: ['impacts', id, days, pilot],
    queryFn: () => api.stationImpacts(id, { days: windowDays(days), pilot: pilot || undefined }),
  })
  // the pilot list comes from the unfiltered group, so choosing one does not empty the menu
  const all = useQuery({ queryKey: ['impacts', id, days, ''], queryFn: () => api.stationImpacts(id, { days: windowDays(days) }) })
  // the station's name and kind come from the live picture
  const live = useQuery({ queryKey: ['live'], queryFn: () => api.live(), staleTime: 60_000 })
  const station = live.data?.live?.stations.find(s => s.id === id)
  const pilots = useMemo(() => {
    const m = new Map<string, string>()
    all.data?.impacts.forEach(i => i.ucid && m.set(i.ucid, i.name))
    return [...m].sort((a, b) => a[1].localeCompare(b[1]))
  }, [all.data])
  const weapons = useMemo(() => [...new Set(q.data?.impacts.map(i => i.weapon) ?? [])].sort(), [q.data])

  const impacts = useMemo(() => (q.data?.impacts ?? []).filter(i => !weapon || i.weapon === weapon), [q.data, weapon])
  const cep = useMemo(() => {
    if (!weapon) return q.data?.cep_m ?? null
    const s = impacts.map(i => i.miss_m).sort((a, b) => a - b)
    if (!s.length) return null
    const m = Math.floor(s.length / 2)
    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2
  }, [impacts, q.data, weapon])
  const mpi = useMemo(() => {
    if (!impacts.length) return null
    return {
      north: impacts.reduce((a, i) => a + i.north_m, 0) / impacts.length,
      east: impacts.reduce((a, i) => a + i.east_m, 0) / impacts.length,
    }
  }, [impacts])
  const byQuality = useMemo(() => QUALITIES.map(ql => ({ q: ql, n: impacts.filter(i => i.quality === ql).length })), [impacts])
  const sorted = useMemo(() => [...impacts].sort((a, b) => b.ts.localeCompare(a.ts)), [impacts])

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <div className="caps"><Link to="/results" className="hover:underline">Results</Link> · Impact group</div>
          <h1 className="display mt-1">{station?.name ?? id}</h1>
          <p className="sub m-0 mt-1 mono text-[12px]">
            {id}
            {q.data?.target && <> · {q.data.target.lat.toFixed(4)}°N {q.data.target.lon.toFixed(4)}°E · {fmt(mToFt(q.data.target.alt_m))} ft</>}
            {station?.laser_code != null && <> · laser {station.laser_code}</>}
          </p>
        </div>
        <div className="actions">
          <Link to={`/calc?tool=bomb&station=${encodeURIComponent(id)}`} className="btn-range sm">Release calculator</Link>
          <DaysSeg value={days} onChange={setDays} />
        </div>
      </div>

      <div className="flex flex-wrap items-end gap-3 mb-4">
        <Select label="Pilot" value={pilot} onChange={setPilot} options={[{ value: '', label: 'Everyone' }, ...pilots.map(([u, n]) => ({ value: u, label: n }))]} />
        <Select label="Weapon" value={weapon} onChange={setWeapon} options={[{ value: '', label: 'All weapons' }, ...weapons.map(w => ({ value: w, label: weaponName(w) }))]} />
      </div>

      {q.isLoading ? (
        <Loading label="Plotting impacts" />
      ) : q.error ? (
        <ErrorState error={q.error} retry={() => q.refetch()} />
      ) : !impacts.length ? (
        <div className="panel"><Empty title="No impacts here yet">Nobody has dropped on this target in the selected window.</Empty></div>
      ) : (
        <div className="grid gap-4 lg:grid-cols-[minmax(0,640px)_minmax(0,1fr)]">
          <div className="panel panel-b">
            <BombPlot
              impacts={impacts.map(i => ({ ...i, label: `${i.name} · ${weaponName(i.weapon)} · ${fmtDateTime(i.ts)}` }))}
              rings={q.data?.rings_m ?? []}
              goodRadius={SCORING_DEFAULTS.good_unguided_m}
              cep={cep}
              mpi={mpi}
              linkable
            />
          </div>
          <div className="flex flex-col gap-3 min-w-0">
            <div className="panel panel-b grid grid-cols-3 gap-3">
              <div className="kpi"><span className="caps">Impacts</span><span className="v">{impacts.length}</span></div>
              <div className="kpi"><span className="caps">CEP</span><span className="v" style={{ color: 'var(--ball)' }}>{fmt(cep, 1)}<small>m</small></span></div>
              <div className="kpi">
                <span className="caps">Mean point</span>
                <span className="v text-[15px]!">{mpi ? `${fmt(Math.hypot(mpi.north, mpi.east), 1)} m` : '—'}</span>
                {mpi && <span className="mono text-[11px] dim">N {fmt(mpi.north, 1)} · E {fmt(mpi.east, 1)}</span>}
              </div>
            </div>
            <Panel title="Grades">
              <div className="flex flex-col gap-1.5">
                {byQuality.map(({ q: ql, n }) => (
                  <div key={ql} className="grid grid-cols-[100px_1fr_40px] items-center gap-2 text-[12px]">
                    <QualityChip q={ql} />
                    <div className="h-2 rounded-[1px]" style={{ background: 'var(--plot-grid)' }}>
                      <div className="h-full rounded-[1px]" style={{ width: `${(n / impacts.length) * 100}%`, background: TONE_VAR[qualityTone(ql)] }} />
                    </div>
                    <span className="mono text-right">{n}</span>
                  </div>
                ))}
              </div>
              <p className="text-[11.5px] dim mt-3 mb-0">
                Unguided bands (default): SHACK ≤ {SCORING_DEFAULTS.shack_m} m, then {bombBands(SCORING_DEFAULTS.good_unguided_m).slice(1).map(b => `${b.quality} ≤ ${fmt(b.r, 1)} m`).join(', ')}, POOR beyond.
                Guided weapons are graded against {SCORING_DEFAULTS.good_guided_m} m, rockets against {SCORING_DEFAULTS.good_rocket_m} m.
              </p>
            </Panel>
            <Panel title="Impacts" bodyClass="table-scroll max-h-[420px] overflow-y-auto">
              <table className="table-range">
                <thead>
                  <tr><th>When</th><th>Pilot</th><th>Weapon</th><th className="n">Miss</th><th>Grade</th></tr>
                </thead>
                <tbody>
                  {sorted.map(i => (
                    <tr key={i.id}>
                      <td className="mono text-[12px] whitespace-nowrap"><Link to={`/result/${encodeURIComponent(i.id)}`} className="hover:underline">{fmtDateTime(i.ts)}</Link></td>
                      <td>{i.ucid ? <Link to={`/pilot/${encodeURIComponent(i.ucid)}`} className="hover:underline">{i.name}</Link> : i.name}</td>
                      <td className="mono text-[12px]">{weaponName(i.weapon)}</td>
                      <td className="n">{fmt(i.miss_m, 1)} m</td>
                      <td><QualityChip q={i.quality} /></td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </Panel>
          </div>
        </div>
      )}
    </div>
  )
}
