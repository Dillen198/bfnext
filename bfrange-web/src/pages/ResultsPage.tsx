/**
 * `/results` — every result card, filterable by discipline, airframe,
 * station, pilot and time window. Filters live in the URL so a filtered view
 * can be shared.
 */
import { useEffect, useMemo, useState } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { keepPreviousData, useInfiniteQuery, useQuery } from '@tanstack/react-query'
import { X } from '@icons'
import { api } from '../api'
import { DaysSeg, Panel, Select } from '../components/Controls'
import { ResultCard } from '../components/ResultCard'
import { Empty, ErrorState, Loading } from '../components/States'
import { KIND_LABEL, STATION_KIND_LABEL, airframe } from '../lib/format'
import { RESULT_KINDS, type ResultKind } from '../types'

const PAGE = 30
const AIRFRAMES = ['FA-18C_hornet', 'F-14B', 'F-16C_50', 'A-10C_2', 'AV8BNA', 'F-15ESE', 'T-45', 'M-2000C', 'UH-1H', 'CH-47Fbl1', 'AH-64D_BLK_II', 'Mi-8MT', 'Ka-50_3']
const GROUPABLE = ['bomb_circle', 'tactical_array', 'laser_target', 'coord_target', 'convoy']

function useDebounced<T>(v: T, ms = 250): T {
  const [d, setD] = useState(v)
  useEffect(() => {
    const t = setTimeout(() => setD(v), ms)
    return () => clearTimeout(t)
  }, [v, ms])
  return d
}

export default function ResultsPage() {
  const [sp, setSp] = useSearchParams()
  const kind = (sp.get('kind') ?? '') as ResultKind | ''
  const unitType = sp.get('unit_type') ?? ''
  const station = sp.get('station') ?? ''
  const pilot = sp.get('pilot') ?? ''
  const pilotName = sp.get('pilot_name') ?? ''
  const days = Number(sp.get('days') ?? 30)
  const set = (k: string, v: string | number) => {
    const n = new URLSearchParams(sp)
    if (v === '' || v === 0 && k !== 'days') n.delete(k)
    else n.set(k, String(v))
    setSp(n, { replace: true })
  }

  const [pilotText, setPilotText] = useState('')
  const needle = useDebounced(pilotText)
  const hits = useQuery({ queryKey: ['pilots', needle], queryFn: () => api.pilots(needle), enabled: needle.length > 0 })
  const live = useQuery({ queryKey: ['live'], queryFn: () => api.live(), staleTime: 60_000 })
  const stations = live.data?.live?.stations ?? []

  const filters = { kind: kind || undefined, unit_type: unitType || undefined, station: station || undefined, pilot: pilot || undefined, days: days || undefined }
  const q = useInfiniteQuery({
    queryKey: ['results', filters],
    queryFn: ({ pageParam }) => api.results({ ...filters, limit: PAGE, offset: pageParam }),
    initialPageParam: 0,
    getNextPageParam: (last, pages) => {
      const got = pages.reduce((a, p) => a + p.items.length, 0)
      return got < last.total ? got : undefined
    },
    placeholderData: keepPreviousData,
  })
  const items = useMemo(() => q.data?.pages.flatMap(p => p.items) ?? [], [q.data])
  const total = q.data?.pages[0]?.total ?? 0

  function pickPilot(text: string) {
    setPilotText(text)
    const hit = hits.data?.find(h => h.name.toLowerCase() === text.trim().toLowerCase())
    if (hit) {
      const n = new URLSearchParams(sp)
      n.set('pilot', hit.ucid)
      n.set('pilot_name', hit.name)
      setSp(n, { replace: true })
      setPilotText('')
    }
  }

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">Range results</h1>
          <p className="sub m-0 mt-1">Every graded event on the range. Open any card for its debrief.</p>
        </div>
        <div className="actions">
          <DaysSeg value={days} onChange={d => set('days', d)} />
        </div>
      </div>

      <div className="grid gap-4 lg:grid-cols-[minmax(0,1fr)_300px]">
        <div className="min-w-0">
          <div className="flex flex-wrap items-end gap-3 mb-3">
            <Select
              label="Discipline"
              value={kind}
              onChange={v => set('kind', v)}
              options={[{ value: '' as ResultKind | '', label: 'All disciplines' }, ...RESULT_KINDS.map(k => ({ value: k as ResultKind | '', label: KIND_LABEL[k] }))]}
            />
            <Select
              label="Airframe"
              value={unitType}
              onChange={v => set('unit_type', v)}
              options={[{ value: '', label: 'All airframes' }, ...AIRFRAMES.map(a => ({ value: a, label: airframe(a) }))]}
            />
            <Select
              label="Station"
              value={station}
              onChange={v => set('station', v)}
              options={[{ value: '', label: 'All stations' }, ...stations.map(s => ({ value: s.id, label: s.name }))]}
            />
            <label className="field min-w-[180px]">
              <span>Pilot</span>
              {pilot ? (
                <span className="input-range flex items-center gap-2" style={{ fontFamily: 'var(--font-body)' }}>
                  <span className="truncate">{pilotName || pilot}</span>
                  <button className="ml-auto muted hover:text-[var(--chalk)]" aria-label="Clear pilot filter" onClick={() => {
                    const n = new URLSearchParams(sp)
                    n.delete('pilot'); n.delete('pilot_name')
                    setSp(n, { replace: true })
                  }}><X size={13} /></button>
                </span>
              ) : (
                <>
                  <input className="input-range" style={{ fontFamily: 'var(--font-body)' }} list="pilot-hits" placeholder="Search a name"
                    value={pilotText} onChange={e => pickPilot(e.target.value)} />
                  <datalist id="pilot-hits">
                    {hits.data?.map(h => <option key={h.ucid} value={h.name}>{h.count} results</option>)}
                  </datalist>
                </>
              )}
            </label>
            <span className="ml-auto mono text-[12px] muted self-center">{total.toLocaleString('en-US')} results</span>
          </div>

          {q.isLoading ? (
            <Loading label="Loading results" />
          ) : q.error ? (
            <ErrorState error={q.error} retry={() => q.refetch()} />
          ) : !items.length ? (
            <div className="panel"><Empty title="No results match">Try a longer time window or fewer filters.</Empty></div>
          ) : (
            <>
              <div className="grid gap-1.5 md:grid-cols-2" style={{ opacity: q.isPlaceholderData ? 0.6 : 1 }}>
                {items.map(s => <ResultCard key={s.id} s={s} />)}
              </div>
              {q.hasNextPage && (
                <div className="flex justify-center mt-4">
                  <button className="btn-range" onClick={() => q.fetchNextPage()} disabled={q.isFetchingNextPage}>
                    {q.isFetchingNextPage ? 'Loading…' : `Show more (${total - items.length} left)`}
                  </button>
                </div>
              )}
            </>
          )}
        </div>

        <aside className="flex flex-col gap-3">
          <Panel title="Impact groups" bodyClass="">
            <p className="text-[12.5px] muted px-3 pt-2.5 m-0">Every drop on one target, with the group's CEP and mean point of impact.</p>
            <ul className="m-0 p-0 list-none mt-1">
              {stations.filter(s => GROUPABLE.includes(s.kind)).map(s => (
                <li key={s.id} className="border-t border-[var(--line)]">
                  <Link to={`/stations/${encodeURIComponent(s.id)}`} className="flex items-center gap-2 px-3 py-2 hover:bg-[var(--panel-2)]">
                    <span className="text-[13px] font-medium truncate">{s.name}</span>
                    <span className="text-[11.5px] dim ml-auto shrink-0">{STATION_KIND_LABEL[s.kind]}</span>
                  </Link>
                </li>
              ))}
              {!stations.length && <li className="px-3 py-2 text-[12.5px] dim">Station list comes from the live range; it is offline.</li>}
            </ul>
          </Panel>
        </aside>
      </div>
    </div>
  )
}
