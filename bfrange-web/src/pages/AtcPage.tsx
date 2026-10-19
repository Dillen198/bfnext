/** `/atc` — the spoken ATC's radio, frequency by frequency, as it happens. */
import { useEffect, useMemo, useRef, useState } from 'react'
import { API_ROOT, withInstance } from '../api'
import { Empty, ErrorState, Loading } from '../components/States'

interface Station {
  id: string
  field: string
  position: string
  name: string
  freq: string
}

interface Line {
  t: number
  station: string
  who: string
  text: string
  pilot: boolean
  ai: boolean
}

interface Transcript {
  stations: Station[]
  lines: Line[]
}

const KEEP = 600
const POLL_MS = 2000
/** How often to look again when the server has no ATC (or no route for it). */
const OFF_POLL_MS = 60_000

/** The server answered, but has no ATC: an older bfdb without the route. */
class NoAtc extends Error {}

async function fetchTranscript(since: number): Promise<Transcript> {
  const r = await fetch(withInstance(`${API_ROOT}/api/range/atc?since=${since}`), { credentials: 'include' })
  if (r.status === 404) throw new NoAtc('no ATC on this server')
  if (!r.ok) throw new Error(`ATC transcript: ${r.status} ${r.statusText}`)
  return r.json()
}

const clock = (t: number) =>
  new Date(t).toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false })

/** New lines since the page was opened, and the ones in the last minute. */
function useTranscript() {
  const [stations, setStations] = useState<Station[] | null>(null)
  const [lines, setLines] = useState<Line[]>([])
  const [error, setError] = useState<unknown>(null)
  const since = useRef(0)
  useEffect(() => {
    let dead = false
    let timer: ReturnType<typeof setTimeout> | undefined
    const tick = async () => {
      let wait = POLL_MS
      try {
        const d = await fetchTranscript(since.current)
        if (dead) return
        setStations(d.stations)
        setError(null)
        if (d.lines.length) {
          since.current = d.lines[d.lines.length - 1].t
          setLines(prev => [...prev, ...d.lines].slice(-KEEP))
        }
        // Nothing configured: no point asking every two seconds.
        if (!d.stations.length) wait = OFF_POLL_MS
      } catch (e) {
        if (dead) return
        if (e instanceof NoAtc) {
          setStations([])
          setError(null)
        } else {
          setError(e)
        }
        // Back off while the server has no ATC or is unreachable.
        wait = e instanceof NoAtc ? OFF_POLL_MS : POLL_MS * 5
      }
      if (!dead) timer = setTimeout(tick, document.hidden ? Math.max(wait, POLL_MS * 5) : wait)
    }
    tick()
    return () => {
      dead = true
      if (timer) clearTimeout(timer)
    }
  }, [])
  return { stations, lines, error }
}

function Lines({ lines, stations, showStation }: { lines: Line[]; stations: Station[]; showStation: boolean }) {
  const end = useRef<HTMLDivElement>(null)
  const box = useRef<HTMLDivElement>(null)
  // Follow the conversation, unless the reader has scrolled up to read back.
  useEffect(() => {
    const b = box.current
    if (!b) return
    const atBottom = b.scrollHeight - b.scrollTop - b.clientHeight < 120
    if (atBottom) end.current?.scrollIntoView({ block: 'end' })
  }, [lines.length])
  const byId = useMemo(() => new Map(stations.map(s => [s.id, s])), [stations])
  if (!lines.length) {
    return (
      <Empty title="Quiet on frequency">
        Nothing said yet. Call Andersen on SRS, or wait for the AI traffic: it shows up here as it is spoken.
      </Empty>
    )
  }
  return (
    <div ref={box} className="panel-b" style={{ maxHeight: '68vh', overflowY: 'auto' }}>
      {lines.map((l, i) => {
        const st = byId.get(l.station)
        return (
          <div key={`${l.t}-${i}`} className="flex gap-3 py-1.5" style={{ borderBottom: '1px solid var(--line, rgba(127,127,127,.12))' }}>
            <span className="mono dim shrink-0 text-[12px] pt-0.5" style={{ width: 64 }}>{clock(l.t)}</span>
            {showStation && (
              <span className="mono shrink-0 text-[12px] pt-0.5" style={{ width: 150, color: 'var(--dim)' }} title={st ? `${st.name} ${st.freq}` : l.station}>
                {st ? `${st.position.toUpperCase()} ${st.freq}` : l.station}
              </span>
            )}
            <div className="min-w-0">
              <span className="font-semibold" style={{ color: l.pilot ? undefined : 'var(--ball)' }}>
                {l.who}
                {l.ai && <span className="mono text-[10px] ml-1.5 px-1 rounded" style={{ background: 'var(--ball-soft)', color: 'var(--dim)' }}>AI</span>}
              </span>
              <span className="dim">: </span>
              <span>{l.text}</span>
            </div>
          </div>
        )
      })}
      <div ref={end} />
    </div>
  )
}

export default function AtcPage() {
  const { stations, lines, error } = useTranscript()
  const [tab, setTab] = useState<string>('all')
  const now = Date.now()
  const shown = tab === 'all' ? lines : lines.filter(l => l.station === tab)
  // A frequency someone has spoken on in the last minute.
  const busy = (id: string) => lines.some(l => l.station === id && now - l.t < 60_000)

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">ATC radio</h1>
          <p className="sub m-0 mt-1">
            Every transmission on Andersen's frequencies: controllers, players and AI traffic, as it is said.
            Tune these in SRS to talk to them.
          </p>
        </div>
      </div>
      {stations === null && !error ? (
        <Loading label="Tuning in" />
      ) : error && !stations ? (
        <ErrorState error={error} retry={() => location.reload()} />
      ) : !stations?.length ? (
        <Empty title="ATC is off">
          The range's spoken ATC is not running on this server yet. This page checks again every minute.
        </Empty>
      ) : (
        <>
          <div className="tabs-range mb-3" role="tablist">
            <button role="tab" aria-selected={tab === 'all'} onClick={() => setTab('all')}>All</button>
            {stations.map(s => (
              <button key={s.id} role="tab" aria-selected={tab === s.id} onClick={() => setTab(s.id)} title={s.name}>
                {busy(s.id) && <span className="dot mr-1.5" style={{ display: 'inline-block', width: 7, height: 7, borderRadius: 9, background: 'var(--datum)' }} />}
                {s.position === 'atis' ? 'ATIS' : s.position[0].toUpperCase() + s.position.slice(1)}
                <span className="mono dim ml-1.5 text-[11px]">{s.freq}</span>
              </button>
            ))}
          </div>
          <div className="panel">
            <Lines lines={shown} stations={stations} showStation={tab === 'all'} />
          </div>
        </>
      )}
    </div>
  )
}
