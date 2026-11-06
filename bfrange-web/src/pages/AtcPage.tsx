/** `/atc` — the spoken ATC's radio, frequency by frequency, as it happens. */
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { API_ROOT, withInstance } from '../api'
import { Empty, ErrorState, Loading, Offline } from '../components/States'
import { fetchWithSnapshot, useOffline } from '@shared/web/snapshot'

interface Station {
  id: string
  /** The facility: "Andersen", "Lincoln CATCC", "Texaco 1-1", "Range Control". */
  field: string
  position: string
  /** What the controller calls itself: "Lincoln Marshal". */
  name: string
  /** The tab's name: "Tower", "Lincoln Marshal", "Texaco 1-1". */
  label?: string
  freq: string
}

/** A station's short name for tabs and the transcript. */
const tabName = (s: Station) =>
  s.label || (s.position === 'atis' ? 'ATIS' : s.position[0].toUpperCase() + s.position.slice(1))

/** What a frequency is remembered by between visits (ids change when bfdb restarts). */
const scanKey = (s: Station) => `${s.name}|${s.freq}`

/** The frequencies picked to listen to; null = all of them. */
function loadScan(): string[] | null {
  try {
    const v = JSON.parse(localStorage.getItem('atc-scan') ?? 'null')
    return Array.isArray(v) && v.length ? v.map(String) : null
  } catch {
    return null
  }
}

function saveScan(v: string[] | null) {
  try {
    if (v) localStorage.setItem('atc-scan', JSON.stringify(v))
    else localStorage.removeItem('atc-scan')
  } catch { /* storage blocked */ }
}

interface Line {
  t: number
  station: string
  who: string
  text: string
  pilot: boolean
  ai: boolean
  /** The aircraft it is from or to ("C-17"). */
  aircraft?: string
  /** Its audio: /api/range/atc/audio/<id>. */
  audio?: number
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
  // Through the snapshot wrapper only so an outage shows as one (there is no
  // snapshot of the transcript).
  const r = await fetchWithSnapshot(withInstance(`${API_ROOT}/api/range/atc?since=${since}`), '/api/range/atc', { credentials: 'include' })
  if (r.status === 404) throw new NoAtc('no ATC on this server')
  if (!r.ok) throw new Error(`ATC transcript: ${r.status} ${r.statusText}`)
  return r.json()
}

const audioUrl = (id: number) => withInstance(`${API_ROOT}/api/range/atc/audio/${id}`)

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

/** Band-limit to a radio's 300-3400 Hz with a touch of drive: how it sounds in the cockpit. */
function radioChain(ctx: AudioContext, volume: number) {
  const hp = ctx.createBiquadFilter()
  hp.type = 'highpass'
  hp.frequency.value = 300
  const lp = ctx.createBiquadFilter()
  lp.type = 'lowpass'
  lp.frequency.value = 3400
  const drive = ctx.createWaveShaper()
  const curve = new Float32Array(1024)
  for (let i = 0; i < curve.length; i++) {
    const x = (i / (curve.length - 1)) * 2 - 1
    curve[i] = Math.tanh(1.6 * x) / Math.tanh(1.6)
  }
  drive.curve = curve
  const gain = ctx.createGain()
  gain.gain.value = volume
  hp.connect(lp).connect(drive).connect(gain).connect(ctx.destination)
  return { input: hp as AudioNode, gain }
}

/** A burst of static, fading out: the squelch tail at the end of a transmission. */
function squelch(ctx: AudioContext, to: AudioNode, at: number, seconds = 0.12) {
  const n = Math.floor(ctx.sampleRate * seconds)
  const buf = ctx.createBuffer(1, n, ctx.sampleRate)
  const d = buf.getChannelData(0)
  for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * 0.12 * (1 - i / n)
  const src = ctx.createBufferSource()
  src.buffer = buf
  src.connect(to)
  src.start(at)
}

/** Replays one past transmission at a time (the ▶ on each line). */
class ClipPlayer {
  private ctx: AudioContext | null = null
  private input: AudioNode | null = null
  private gain: GainNode | null = null
  private current: AudioBufferSourceNode | null = null
  private volume = 0.8
  private onPlaying: (id: number | null) => void
  constructor(onPlaying: (id: number | null) => void) {
    this.onPlaying = onPlaying
  }

  private ensure(): AudioContext {
    if (!this.ctx) {
      this.ctx = new AudioContext()
      const c = radioChain(this.ctx, this.volume)
      this.input = c.input
      this.gain = c.gain
    }
    void this.ctx.resume()
    return this.ctx
  }

  setVolume(v: number) {
    this.volume = v
    if (this.gain) this.gain.gain.value = v
  }

  stop() {
    this.current?.stop()
    this.current = null
    this.onPlaying(null)
  }

  async play(id: number) {
    this.stop()
    const ctx = this.ensure()
    try {
      const r = await fetch(audioUrl(id), { credentials: 'include' })
      if (!r.ok) throw new Error(String(r.status))
      const buf = await ctx.decodeAudioData(await r.arrayBuffer())
      const src = ctx.createBufferSource()
      src.buffer = buf
      src.connect(this.input!)
      src.onended = () => {
        if (this.current === src) {
          squelch(ctx, this.input!, ctx.currentTime)
          this.current = null
          this.onPlaying(null)
        }
      }
      this.current = src
      this.onPlaying(id)
      src.start()
    } catch {
      this.onPlaying(null) // aged out on the server, or not playable
    }
  }
}

/** Who is talking on which frequency right now. */
type OnAir = Record<string, { who: string; text: string }>

/**
 * The live feed: the frequencies picked (or all of them) as they happen, the
 * way LiveATC streams a real airport. Audio arrives over a WebSocket in 100 ms
 * chunks and is scheduled back to back per frequency, with a little buffer
 * against network jitter; between transmissions there is only silence.
 */
class LiveFeed {
  private ctx: AudioContext
  private input: AudioNode
  private gain: GainNode
  private ws: WebSocket | null = null
  private next = new Map<string, number>()
  private onAir: OnAir = {}
  private station = 'all'
  private skip: (station: string) => boolean = () => false
  private live = false
  private retry: ReturnType<typeof setTimeout> | undefined
  private onChange: (a: OnAir, connected: boolean) => void

  constructor(volume: number, onChange: (a: OnAir, connected: boolean) => void) {
    this.onChange = onChange
    this.ctx = new AudioContext()
    const c = radioChain(this.ctx, volume)
    this.input = c.input
    this.gain = c.gain
  }

  setVolume(v: number) {
    this.gain.gain.value = v
  }

  /** Call inside the click that starts listening: browsers only allow audio after one. */
  unlock() {
    void this.ctx.resume()
  }

  /**
   * Tune to some frequencies (comma-separated ids, or `all`), reconnecting
   * only when that changes. `skip` filters on this side (ATIS on "all").
   */
  start(station: string, skip: (station: string) => boolean) {
    this.skip = skip
    const again = !this.live || station !== this.station || !this.ws
    this.station = station
    this.live = true
    if (again) this.connect()
  }

  stop() {
    this.live = false
    clearTimeout(this.retry)
    this.ws?.close()
    this.ws = null
    this.onAir = {}
    this.onChange({}, false)
  }

  close() {
    this.stop()
    void this.ctx.close()
  }

  private connect() {
    this.ws?.close()
    const url = withInstance(`${API_ROOT}/api/range/atc/live?station=${encodeURIComponent(this.station)}`).replace(/^http/, 'ws')
    const ws = new WebSocket(url)
    ws.binaryType = 'arraybuffer'
    this.ws = ws
    ws.onopen = () => this.onChange(this.onAir, true)
    ws.onclose = () => {
      if (this.ws !== ws) return
      this.onChange({}, false)
      if (this.live) this.retry = setTimeout(() => this.connect(), 3000)
    }
    ws.onmessage = e => (typeof e.data === 'string' ? this.control(e.data) : this.audio(e.data as ArrayBuffer))
  }

  private control(json: string) {
    let m: { on?: string; off?: string; who?: string; text?: string }
    try {
      m = JSON.parse(json)
    } catch {
      return
    }
    if (m.on && !this.skip(m.on)) {
      this.onAir = { ...this.onAir, [m.on]: { who: m.who ?? '', text: m.text ?? '' } }
      this.onChange(this.onAir, true)
    } else if (m.off) {
      const at = Math.max(this.next.get(m.off) ?? 0, this.ctx.currentTime)
      if (this.onAir[m.off]) squelch(this.ctx, this.input, at)
      // the indicator goes out when the audio does, not when the last chunk arrives
      const station = m.off
      setTimeout(() => {
        if (!this.onAir[station]) return
        const rest = { ...this.onAir }
        delete rest[station]
        this.onAir = rest
        this.onChange(this.onAir, true)
      }, Math.max(0, (at - this.ctx.currentTime) * 1000))
    }
  }

  private audio(buf: ArrayBuffer) {
    const b = new Uint8Array(buf)
    const n = b[0]
    const station = new TextDecoder().decode(b.subarray(1, 1 + n))
    if (this.skip(station)) return
    // copy out: the samples need 2-byte alignment
    const samples = new Int16Array(b.slice(1 + n).buffer)
    if (!samples.length) return
    const a = this.ctx.createBuffer(1, samples.length, 8000)
    const d = a.getChannelData(0)
    for (let i = 0; i < samples.length; i++) d[i] = samples[i] / 32768
    const now = this.ctx.currentTime
    let at = this.next.get(station) ?? 0
    // fallen behind (or a new transmission): start a little ahead, to ride out jitter
    if (at < now + 0.05) at = now + 0.35
    const src = this.ctx.createBufferSource()
    src.buffer = a
    src.connect(this.input)
    src.start(at)
    this.next.set(station, at + a.duration)
  }
}

function Lines({
  lines,
  stations,
  showStation,
  playing,
  onPlay,
}: {
  lines: Line[]
  stations: Station[]
  showStation: boolean
  playing: number | null
  onPlay: (id: number) => void
}) {
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
        const now = playing !== null && l.audio === playing
        return (
          <div
            key={`${l.t}-${i}`}
            className="flex gap-3 py-1.5 items-start"
            style={{
              borderBottom: '1px solid var(--line, rgba(127,127,127,.12))',
              background: now ? 'var(--ball-soft)' : undefined,
            }}
          >
            <span className="mono dim shrink-0 text-[12px] pt-0.5" style={{ width: 64 }}>{clock(l.t)}</span>
            {showStation && (
              <span className="mono shrink-0 text-[12px] pt-0.5" style={{ width: 150, color: 'var(--dim)' }} title={st ? `${st.name} ${st.freq}` : l.station}>
                {st ? `${tabName(st).toUpperCase()} ${st.freq}` : l.station}
              </span>
            )}
            <div className="min-w-0 flex-1">
              <span className="font-semibold" style={{ color: l.pilot ? undefined : 'var(--ball)' }}>
                {l.who}
              </span>
              {l.pilot && l.aircraft && (
                <span className="mono text-[10px] ml-1.5 px-1 rounded" style={{ border: '1px solid var(--dim)', color: 'var(--dim)' }} title="Aircraft type">
                  {l.aircraft}
                </span>
              )}
              {l.ai && <span className="mono text-[10px] ml-1.5 px-1 rounded" style={{ background: 'var(--ball-soft)', color: 'var(--dim)' }}>AI</span>}
              {!l.pilot && l.aircraft && <span className="mono dim text-[11px] ml-1.5">to {l.aircraft}</span>}
              <span className="dim">: </span>
              <span>{l.text}</span>
            </div>
            <span className="shrink-0" style={{ width: 28 }}>
              {l.audio !== undefined && (
                <button
                  className="btn-range ghost sm"
                  onClick={() => onPlay(l.audio!)}
                  aria-label={`Play ${l.who}'s transmission`}
                  title="Play this transmission"
                  style={{ padding: '0 6px', lineHeight: '20px' }}
                >
                  {now ? '■' : '▶'}
                </button>
              )}
            </span>
          </div>
        )
      })}
      <div ref={end} />
    </div>
  )
}

function loadVolume(): number {
  try {
    const v = Number(localStorage.getItem('atc-volume'))
    return v > 0 && v <= 1 ? v : 0.8
  } catch {
    return 0.8
  }
}

export default function AtcPage() {
  const { stations, lines, error } = useTranscript()
  const { offline } = useOffline()
  const [tab, setTab] = useState<string>('all')
  const [listen, setListen] = useState(false)
  const [connected, setConnected] = useState(false)
  const [onAir, setOnAir] = useState<OnAir>({})
  const [withAtis, setWithAtis] = useState(false)
  /** The frequencies picked to listen to, by scanKey; null = all. */
  const [scan, setScan] = useState<string[] | null>(loadScan)
  const [volume, setVolume] = useState(loadVolume)
  const [playing, setPlaying] = useState<number | null>(null)
  const clips = useMemo(() => new ClipPlayer(setPlaying), [])
  const feed = useRef<LiveFeed | null>(null)
  // The clock the "spoken in the last minute" dots read, ticking every 10 s.
  const [now, setNow] = useState(() => Date.now())
  useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 10_000)
    return () => clearInterval(t)
  }, [])

  useEffect(
    () => () => {
      clips.stop()
      feed.current?.close()
    },
    [clips],
  )
  useEffect(() => {
    clips.setVolume(volume)
    feed.current?.setVolume(volume)
    try {
      localStorage.setItem('atc-volume', String(volume))
    } catch { /* storage blocked */ }
  }, [clips, volume])

  // The ids of the frequencies picked; ones not on the air right now drop out.
  const tuned = useMemo(() => {
    if (!scan || !stations) return null
    const ids = stations.filter(s => scan.includes(scanKey(s))).map(s => s.id)
    return ids.length ? ids : null
  }, [scan, stations])
  // On "all", ATIS repeats every minute and would drown the rest out.
  const skip = useCallback((station: string) => !tuned && !withAtis && station.endsWith(':atis'), [tuned, withAtis])
  const tunedParam = tuned ? tuned.join(',') : 'all'
  // Retune when the picked frequencies (or the ATIS choice) change while listening.
  useEffect(() => {
    if (listen) feed.current?.start(tunedParam, skip)
  }, [listen, tunedParam, skip])
  const pickAll = () => {
    setScan(null)
    saveScan(null)
  }
  /** Add or drop one frequency; from "all", picking one listens to just that one. */
  const pick = (s: Station) => {
    const k = scanKey(s)
    const cur = tuned ? (stations ?? []).filter(x => tuned.includes(x.id)).map(scanKey) : []
    const next = cur.includes(k) ? cur.filter(x => x !== k) : [...cur, k]
    const v = next.length ? next : null
    setScan(v)
    saveScan(v)
  }

  const toggleListen = () => {
    if (listen) {
      feed.current?.stop()
      setListen(false)
      return
    }
    // made inside the click, so the browser lets it play
    if (!feed.current) {
      feed.current = new LiveFeed(volume, (a, c) => {
        setOnAir(a)
        setConnected(c)
      })
    }
    clips.stop()
    feed.current.unlock()
    setListen(true) // the retune effect connects
  }
  const replay = (id: number) => {
    if (playing === id) clips.stop()
    else void clips.play(id)
  }

  const shown = tab === 'all' ? lines : lines.filter(l => l.station === tab)
  // A frequency someone has spoken on in the last minute.
  const busy = (id: string) => lines.some(l => l.station === id && now - l.t < 60_000)
  const byId = new Map((stations ?? []).map(s => [s.id, s]))
  const listening = !tuned
    ? `all frequencies${withAtis ? '' : ' except ATIS'}`
    : tuned.map(id => (byId.get(id) ? `${tabName(byId.get(id)!)} ${byId.get(id)!.freq}` : id)).join(', ')
  const talking = Object.entries(onAir)

  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">ATC radio</h1>
          <p className="sub m-0 mt-1">
            Every transmission on the range's frequencies, as it is said: Andersen's controllers, the carriers'
            Marshal and Paddles, the tankers and Range Control, with players and AI traffic. Tune these in SRS to
            talk to them, or listen in live here.
          </p>
        </div>
        {!!stations?.length && (
          <div className="actions flex items-center gap-3 flex-wrap">
            <button
              className={`btn-range sm ${listen ? '' : 'ghost'}`}
              onClick={toggleListen}
              aria-pressed={listen}
              title={listen ? 'Stop listening' : 'Listen to the frequency live'}
            >
              {listen ? '■ Stop' : '▶ Listen live'}
            </button>
            <label className="flex items-center gap-1.5 text-[12px] dim" title="Volume">
              Vol
              <input type="range" min={0.05} max={1} step={0.05} value={volume} onChange={e => setVolume(Number(e.target.value))} style={{ width: 90 }} />
            </label>

          </div>
        )}
      </div>
      {stations === null && !error ? (
        <Loading label="Tuning in" />
      ) : error && !stations ? (
        offline ? <Offline what="The range's ATC" /> : <ErrorState error={error} retry={() => location.reload()} />
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
                {(onAir[s.id] || busy(s.id)) && (
                  <span
                    className="dot mr-1.5"
                    style={{ display: 'inline-block', width: 7, height: 7, borderRadius: 9, background: onAir[s.id] ? 'var(--wave)' : 'var(--datum)' }}
                  />
                )}
                {tabName(s)}
                <span className="mono dim ml-1.5 text-[11px]">{s.freq}</span>
              </button>
            ))}
          </div>
          <div className="panel mb-3">
            <div className="panel-b">
              <div className="flex items-center gap-2 flex-wrap text-[12px]" role="group" aria-label="Frequencies to listen to">
                <span className="dim mr-1">Listen to</span>
                <button className={`btn-range sm ${tuned ? 'ghost' : ''}`} aria-pressed={!tuned} onClick={pickAll} title="Every frequency at once">
                  All
                </button>
                {stations.map(s => {
                  const on = !!tuned?.includes(s.id)
                  return (
                    <button
                      key={s.id}
                      className={`btn-range sm ${on ? '' : 'ghost'}`}
                      aria-pressed={on}
                      onClick={() => pick(s)}
                      title={`${s.name} ${s.freq}`}
                    >
                      {onAir[s.id] && (
                        <span className="mr-1.5" style={{ display: 'inline-block', width: 7, height: 7, borderRadius: 9, background: 'var(--wave)' }} />
                      )}
                      {tabName(s)}
                      <span className="mono ml-1.5 text-[11px]" style={{ opacity: 0.7 }}>{s.freq}</span>
                    </button>
                  )
                })}
                {!tuned && (
                  <label className="flex items-center gap-1.5 dim ml-1">
                    <input type="checkbox" checked={withAtis} onChange={e => setWithAtis(e.target.checked)} />
                    ATIS too
                  </label>
                )}
              </div>
              {!listen && <div className="dim text-[12px] mt-2">Pick one frequency, several, or all, then press Listen live.</div>}
            </div>
          </div>
          {listen && (
            <div className="panel mb-3">
              <div className="panel-b flex items-center gap-3 flex-wrap text-[13px]">
                <span className="mono text-[11px] px-1.5 rounded" style={{ background: connected ? 'var(--wave-soft)' : 'var(--ball-soft)', color: connected ? 'var(--wave)' : 'var(--dim)' }}>
                  {connected ? 'LIVE' : 'CONNECTING'}
                </span>
                {talking.length ? (
                  talking.map(([id, a]) => (
                    <span key={id}>
                      <span className="mono dim text-[11px] mr-1">{byId.get(id) ? `${tabName(byId.get(id)!).toUpperCase()} ${byId.get(id)!.freq}` : ''}</span>
                      <b>{a.who}</b>
                    </span>
                  ))
                ) : (
                  <span className="dim">Listening to {listening}. Quiet on frequency.</span>
                )}
              </div>
            </div>
          )}
          <div className="panel">
            <Lines lines={shown} stations={stations} showStation={tab === 'all'} playing={playing} onPlay={replay} />
          </div>
        </>
      )}
    </div>
  )
}
