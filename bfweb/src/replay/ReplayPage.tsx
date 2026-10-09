// /replay/:rec -- one Tacview recording, replayed in the browser in 2D or 3D.
// `?focus=<object index>` opens on one flight and follows it; `?t=<ms>`
// opens at a moment (both are what the share button copies).

import { useEffect, useMemo, useRef, useState } from 'react'
import { Link, useParams, useSearchParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { Play, Pause, ChevronLeft, Link as LinkIcon, Eye, EyeOff, Download } from '@icons'
import { API_ROOT } from '../api'
import {
  TrackStore, fetchMeta, fetchHumans, sideHex, objLabel, isAir,
  fmtDuration, fmtClock, gameClock, M_TO_FT, MS_TO_KT,
  type RecMeta, type RecEvent, type RecFlight, type State,
} from './data'
import { ReplayEngine, type EngineUi, type ViewMode, type CamMode } from './engine'
import FlightGraph from './FlightGraph'
import Shots from './Shots'
import { braa, aspectWord, gAt, nearest, machOf, iasOf, flightStats, M_TO_NM } from './analysis'
import './replay.css'

const SPEEDS = [0.25, 0.5, 1, 2, 4, 8, 16, 32, 64]

export default function ReplayPage() {
  const { rec = '' } = useParams<{ rec: string }>()
  const meta = useQuery({ queryKey: ['replay-meta', rec], queryFn: () => fetchMeta(rec), staleTime: Infinity, retry: 1 })
  if (meta.isLoading) return <div className="rp-state">Loading recording…</div>
  if (meta.isError || !meta.data) {
    return (
      <div className="rp-state">
        <div className="rp-state-head">Recording not available</div>
        <div>{String((meta.error as Error)?.message ?? 'unknown error')}</div>
        <Link to="/replay" className="rp-btn">All recordings</Link>
      </div>
    )
  }
  return <Viewer rec={rec} meta={meta.data} />
}

function Viewer({ rec, meta }: { rec: string; meta: RecMeta }) {
  const [params, setParams] = useSearchParams()
  const mapEl = useRef<HTMLDivElement>(null)
  const labelsEl = useRef<HTMLDivElement>(null)
  const engine = useRef<ReplayEngine | null>(null)
  const [ui, setUi] = useState<EngineUi | null>(null)
  const [side, setSide] = useState<'flights' | 'shots' | 'events'>('flights')
  const [sideOpen, setSideOpen] = useState(() => window.innerWidth > 900)
  const [fit, setFit] = useState(true)
  const [copied, setCopied] = useState(false)
  const store = useMemo(() => new TrackStore(rec, meta), [rec, meta])
  // Which flights players flew; null (show everything) on an older bfdb.
  const humansQ = useQuery({ queryKey: ['replay-humans', rec], queryFn: () => fetchHumans(rec), staleTime: 300_000, retry: false })
  const humans = useMemo(() => (humansQ.data ? new Set(humansQ.data) : null), [humansQ.data])

  // Engine lifetime = this recording.
  useEffect(() => {
    if (!mapEl.current || !labelsEl.current) return
    const focus = params.get('focus')
    const fi = focus != null && meta.objects[Number(focus)] ? Number(focus) : null
    const t0 = params.get('t') != null ? Number(params.get('t')) : fi != null ? meta.objects[fi].t0 : 0
    const e = new ReplayEngine(mapEl.current, labelsEl.current, store, 0, meta.duration_ms, t0)
    engine.current = e
    e.onUi = setUi
    if (fi != null) e.setFocus(fi)
    e.kick()
    return () => { e.destroy(); engine.current = null }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [store])

  // Follow/centre within the part of the map the panels leave visible.
  useEffect(() => {
    const apply = () => {
      const wide = window.innerWidth > 900
      engine.current?.setPadding({ top: 60, right: wide ? 260 : 0, bottom: 150, left: sideOpen && wide ? 300 : 0 })
    }
    apply()
    window.addEventListener('resize', apply)
    return () => window.removeEventListener('resize', apply)
  }, [sideOpen, store])

  // Keyboard: space play/pause, arrows step.
  useEffect(() => {
    const onKey = (ev: KeyboardEvent) => {
      const e = engine.current
      if (!e || (ev.target as HTMLElement)?.closest('input, select, textarea')) return
      if (ev.code === 'Space') { ev.preventDefault(); e.toggle() }
      else if (ev.code === 'ArrowRight') e.setTime(e.t + (ev.shiftKey ? 60_000 : 10_000))
      else if (ev.code === 'ArrowLeft') e.setTime(e.t - (ev.shiftKey ? 60_000 : 10_000))
      // frame step, for the moment a missile arrives
      else if (ev.code === 'Period') { e.pause(); e.setTime(e.t + 250) }
      else if (ev.code === 'Comma') { e.pause(); e.setTime(e.t - 250) }
      else if (ev.code === 'Escape') e.setMeasure(null)
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])

  const focus = ui?.focus ?? null
  const fo = focus != null ? meta.objects[focus] : null

  // Keep ?focus= in the URL so a reload or a copied link lands on the flight.
  useEffect(() => {
    if (!ui) return // the engine has not reported yet; leave the URL alone
    const cur = params.get('focus')
    const want = focus != null ? String(focus) : null
    if (cur !== want) {
      const p = new URLSearchParams(params)
      if (want) p.set('focus', want); else p.delete('focus')
      p.delete('t')
      setParams(p, { replace: true })
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [focus, ui == null])

  const t = ui?.t ?? 0
  const range: [number, number] = fit && fo
    ? [Math.max(0, fo.t0 - 30_000), Math.min(meta.duration_ms, fo.t1 + 30_000)]
    : [0, meta.duration_ms]

  const share = async () => {
    const u = new URL(window.location.href)
    u.searchParams.set('t', String(Math.round(t)))
    try { await navigator.clipboard.writeText(u.toString()); setCopied(true); setTimeout(() => setCopied(false), 1600) } catch { /* no clipboard */ }
  }

  const e = engine.current
  const game = gameClock(meta, t)

  return (
    <div className="rp-root">
      <div ref={mapEl} className="rp-map" />
      <div ref={labelsEl} className="rp-labels" />

      {/* ── top bar ── */}
      <div className="rp-top">
        <Link to="/replay" className="rp-icon-btn" title="All recordings"><ChevronLeft size={16} /></Link>
        <div className="rp-title">
          <div className="rp-title-main">{meta.title || 'Recording'}</div>
          <div className="rp-title-sub">
            {new Date(meta.start_ms).toISOString().slice(0, 10)} · {fmtDuration(meta.duration_ms)} · {meta.flights.length} flights
          </div>
        </div>
        <div className="rp-clock">
          <div className="rp-clock-main">{fmtClock(meta.start_ms + t)}</div>
          {game && <div className="rp-clock-sub">mission {game}</div>}
        </div>
        <div className="rp-seg" role="group" aria-label="View">
          {(['2d', '3d'] as ViewMode[]).map(m => (
            <button key={m} className={ui?.mode === m ? 'on' : ''} onClick={() => e?.setMode(m)}>{m.toUpperCase()}</button>
          ))}
        </div>
        {focus != null && (
          <div className="rp-seg" role="group" aria-label="Camera">
            {camModes(ui?.mode ?? '2d', ui?.measure != null).map(([c, label, title]) => (
              <button key={c} className={ui?.cam === c ? 'on' : ''} onClick={() => e?.setCam(c)} title={title}>{label}</button>
            ))}
          </div>
        )}
        <div className="rp-seg" role="group" aria-label="Map">
          <button className={ui?.basemap === 'dark' ? 'on' : ''} onClick={() => e?.setBasemap('dark')}>MAP</button>
          <button className={ui?.basemap === 'sat' ? 'on' : ''} onClick={() => e?.setBasemap('sat')}>SAT</button>
        </div>
        <a
          className="rp-icon-btn"
          href={`${API_ROOT}/api/replay/rec/${encodeURIComponent(rec)}/acmi`}
          title="Download the original recording to open in Tacview"
        >
          <Download size={15} />
        </a>
        <button className="rp-icon-btn" onClick={share} title="Copy a link to this moment">
          <LinkIcon size={15} />
          {copied && <span className="rp-toast">Link copied</span>}
        </button>
      </div>

      {/* ── side panel ── */}
      <button className="rp-side-toggle" onClick={() => setSideOpen(o => !o)}>{sideOpen ? '‹' : 'FLIGHTS ›'}</button>
      {sideOpen && (
        <aside className="rp-side">
          <div className="rp-tabs">
            <button className={side === 'flights' ? 'on' : ''} onClick={() => setSide('flights')}>Flights</button>
            <button className={side === 'shots' ? 'on' : ''} onClick={() => setSide('shots')}>Shots</button>
            <button className={side === 'events' ? 'on' : ''} onClick={() => setSide('events')}>Events</button>
          </div>
          {side === 'flights' && <FlightList meta={meta} humans={humans} t={t} focus={focus} onPick={i => { e?.setFocus(i); if (!fit) setFit(true) }} />}
          {side === 'shots' && (
            <Shots
              meta={meta} store={store} focus={focus} version={store.version}
              onPick={sh => {
                if (!e) return
                if (sh.ev.o != null && sh.ev.o !== focus) e.setFocus(sh.ev.o)
                e.setMeasure(sh.ev.tg ?? null)
                e.setTime(sh.ev.t - 5_000)
              }}
            />
          )}
          {side === 'events' && <EventList meta={meta} t={t} focus={focus} onSeek={x => e?.setTime(x)} />}
          {ui && (
            <div className="rp-filters">
              <Toggle on={ui.opts.labels} label="Labels" onClick={() => e?.setOpts({ labels: !ui.opts.labels })} />
              <Toggle on={ui.opts.weapons} label="Weapons" onClick={() => e?.setOpts({ weapons: !ui.opts.weapons })} />
              <Toggle on={ui.opts.ground} label="Ground" onClick={() => e?.setOpts({ ground: !ui.opts.ground })} />
              <Toggle on={ui.opts.trail > 0} label="Trails" onClick={() => e?.setOpts({ trail: ui.opts.trail > 0 ? 0 : 60_000 })} />
            </div>
          )}
        </aside>
      )}

      {/* ── focus card ── */}
      {fo && focus != null && (
        <div className="rp-focus">
          <div className="rp-focus-head">
            <span className="rp-dot" style={{ background: sideHex(fo.c) }} />
            <span className="rp-focus-name">{objLabel(fo)}</span>
            <button className="rp-icon-btn rp-x" onClick={() => e?.setFocus(null)} title="Stop following">×</button>
          </div>
          {fo.g && <div className="rp-focus-sub">{fo.g}</div>}
          <Telemetry s={ui?.focusState ?? null} alive={t >= fo.t0 && t <= fo.t1} g={focusG(store, focus, t)} />
          {isAir(fo.k) && <FlightSummary meta={meta} store={store} idx={focus} version={store.version} />}
          <Measure
            meta={meta}
            a={ui?.focusState ?? null}
            b={ui?.measureState ?? null}
            measure={ui?.measure ?? null}
            measuring={ui?.measuring ?? false}
            onStart={() => e?.startMeasuring(!ui?.measuring)}
            onClear={() => e?.setMeasure(null)}
          />
        </div>
      )}

      {ui?.mode === '3d' && (
        <a className="rp-credit" href={`${import.meta.env.BASE_URL}models/CREDITS.txt`} target="_blank" rel="noreferrer">
          3D models by Sketchfab artists, CC BY 4.0 (full credits) · terrain: Mapzen/AWS · imagery: Esri
        </a>
      )}

      {/* ── timeline ── */}
      <div className="rp-bottom">
        {fo && focus != null && isAir(fo.k) && (
          <FlightGraph store={store} idx={focus} range={range} t={t} onSeek={x => e?.setTime(x)} version={store.version} />
        )}
        <div className="rp-transport">
          <button className="rp-play" onClick={() => e?.toggle()} aria-label={ui?.playing ? 'Pause' : 'Play'}>
            {ui?.playing ? <Pause size={16} /> : <Play size={16} />}
          </button>
          <select className="rp-speed" value={ui?.speed ?? 1} onChange={ev => e?.setSpeed(Number(ev.target.value))} aria-label="Speed">
            {SPEEDS.map(s => <option key={s} value={s}>{s}×</option>)}
          </select>
          <Timeline meta={meta} range={range} t={t} focus={focus} onSeek={x => e?.setTime(x)} />
          <span className="rp-time">{fmtDuration(t - range[0])} / {fmtDuration(range[1] - range[0])}</span>
          {fo && (
            <button className={`rp-chip ${fit ? 'on' : ''}`} onClick={() => setFit(f => !f)} title="Timeline spans this flight / the whole recording">
              {fit ? 'FLIGHT' : 'ALL'}
            </button>
          )}
          {ui?.buffering && <span className="rp-buffer">loading…</span>}
        </div>
        {ui?.error && <div className="rp-error">{ui.error}</div>}
      </div>
    </div>
  )
}

/** The camera buttons a view offers: [mode, label, tooltip]. */
function camModes(mode: ViewMode, measuring: boolean): [CamMode, string, string][] {
  if (mode === '2d') return [['free', 'FREE', 'Pan the map yourself'], ['follow', 'FOLLOW', 'Keep the aircraft centred']]
  const m: [CamMode, string, string][] = [
    ['free', 'FREE', 'Fly the camera yourself'],
    ['follow', 'ORBIT', 'Orbit the aircraft (drag to look around, wheel to zoom)'],
    ['chase', 'CHASE', 'Behind the aircraft (wheel for distance)'],
    ['cockpit', 'COCKPIT', 'From the pilot seat'],
  ]
  if (measuring) m.push(['padlock', 'PADLOCK', 'Behind the aircraft, looking at the measured target'])
  return m
}

function Toggle({ on, label, onClick }: { on: boolean; label: string; onClick: () => void }) {
  return (
    <button className={`rp-chip ${on ? 'on' : ''}`} onClick={onClick}>
      {on ? <Eye size={11} /> : <EyeOff size={11} />} {label}
    </button>
  )
}

/** G at t from the followed aircraft's whole track, once it has arrived. */
function focusG(store: TrackStore, idx: number, t: number): number | null {
  const s = store.wholeTrack(idx)
  if (!s) return null
  return gAt(s, nearest(s, t))
}

function Telemetry({ s, alive, g }: { s: State | null; alive: boolean; g: number | null }) {
  if (!alive) return <div className="rp-focus-sub">not in the air at this moment</div>
  if (!s) return <div className="rp-focus-sub">loading…</div>
  // DCS records attitude and AGL only; IAS and Mach are worked out from the
  // track (marked ~) when the recording does not carry them.
  const cells: [string, string][] = [
    ['ALT', `${Math.round(s.alt * M_TO_FT).toLocaleString()} ft`],
    ['AGL', s.agl != null ? `${Math.round(s.agl * M_TO_FT).toLocaleString()} ft` : '—'],
    ['VS', `${Math.round(s.vs * M_TO_FT * 60).toLocaleString()} fpm`],
    [s.ias != null ? 'IAS' : 'IAS ~', `${Math.round(iasOf(s) * MS_TO_KT)} kt`],
    ['GS', `${Math.round(s.gs * MS_TO_KT)} kt`],
    [s.mach != null ? 'MACH' : 'MACH ~', machOf(s).toFixed(2)],
    ['HDG', `${String(Math.round(s.hdg) % 360).padStart(3, '0')}°`],
    ['G', g != null ? g.toFixed(1) : '—'],
    s.aoa != null ? ['AOA', `${s.aoa.toFixed(1)}°`] : ['BANK', `${Math.round(s.roll)}°`],
  ]
  return (
    <div className="rp-telemetry">
      {cells.map(([k, v], i) => (
        <div key={i}><span>{k}</span><b>{v}</b></div>
      ))}
    </div>
  )
}

function FlightSummary({ meta, store, idx, version }: { meta: RecMeta; store: TrackStore; idx: number; version: number }) {
  const fl = meta.flights.find(f => f.i === idx)
  const st = useMemo(() => {
    const s = store.wholeTrack(idx)
    return s ? flightStats(s) : null
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [store, idx, version])
  return (
    <div className="rp-summary">
      <div className="rp-summary-row">
        <span>Shots <b>{fl?.shots ?? 0}</b></span>
        <span>Hits <b>{fl?.hits ?? '—'}</b></span>
        <span>Kills <b className="k">{fl?.kills ?? 0}</b></span>
        <span className={fl?.fate === 'destroyed' ? 'x' : ''}>{fl?.fate === 'destroyed' ? 'LOST' : fl?.fate === 'landed' ? 'LANDED' : ''}</span>
      </div>
      {st && (
        <div className="rp-summary-row dim">
          <span>max {Math.round(st.maxAlt * M_TO_FT / 100) * 100} ft</span>
          <span>{Math.round(st.maxSpeed * MS_TO_KT)} kt / M{st.maxMach.toFixed(2)}</span>
          <span>{st.maxG.toFixed(1)} / {st.minG.toFixed(1)} G</span>
          <span>{Math.round(st.distanceM * M_TO_NM)} nm</span>
        </div>
      )}
    </div>
  )
}

function Measure({ meta, a, b, measure, measuring, onStart, onClear }: {
  meta: RecMeta; a: State | null; b: State | null; measure: number | null; measuring: boolean
  onStart: () => void; onClear: () => void
}) {
  if (measure == null) {
    return (
      <button className={`rp-chip rp-measure-btn ${measuring ? 'on' : ''}`} onClick={onStart}
        title="Range, bearing, closure and aspect to another object (or shift-click it)">
        {measuring ? 'CLICK A TARGET…' : 'MEASURE TO…'}
      </button>
    )
  }
  const o = meta.objects[measure]
  const r = a && b ? braa(a, b) : null
  return (
    <div className="rp-braa">
      <div className="rp-focus-head">
        <span className="rp-dot" style={{ background: sideHex(o?.c) }} />
        <span className="rp-focus-name">{o ? objLabel(o) : '?'}</span>
        <button className="rp-icon-btn rp-x" onClick={onClear} title="Stop measuring (Esc)">×</button>
      </div>
      {r ? (
        <div className="rp-telemetry">
          <div><span>RANGE</span><b>{(r.range * M_TO_NM).toFixed(1)} nm</b></div>
          <div><span>BRG</span><b>{String(Math.round(r.bearing) % 360).padStart(3, '0')}°</b></div>
          <div><span>ALT Δ</span><b>{r.dAlt >= 0 ? '+' : ''}{Math.round(r.dAlt * M_TO_FT).toLocaleString()} ft</b></div>
          <div><span>CLOSURE</span><b>{Math.round(r.closure * MS_TO_KT)} kt</b></div>
          <div><span>ASPECT</span><b>{aspectWord(r.aspect)} {Math.round(r.aspect)}°</b></div>
          <div><span>ATA</span><b>{Math.round(r.ata)}°</b></div>
        </div>
      ) : <div className="rp-focus-sub">not both in the picture at this moment</div>}
    </div>
  )
}

function Timeline({ meta, range, t, focus, onSeek }: {
  meta: RecMeta; range: [number, number]; t: number; focus: number | null; onSeek: (t: number) => void
}) {
  const [a, b] = range
  const span = Math.max(1, b - a)
  const marks = useMemo(() => meta.events.filter(ev => {
    if (ev.t < a || ev.t > b) return false
    if (focus == null) return ev.k === 'kill' || ev.k === 'destroyed'
    return ev.o === focus || ev.by === focus || ev.w === focus
  }).slice(0, 400), [meta, a, b, focus])
  return (
    <div className="rp-timeline">
      <div className="rp-marks">
        {marks.map((ev, i) => (
          <span key={i} className={`rp-mark rp-mark-${markClass(ev, focus)}`} style={{ left: `${((ev.t - a) / span) * 100}%` }} />
        ))}
      </div>
      <input
        type="range" min={a} max={b} step={100} value={Math.min(Math.max(t, a), b)}
        onChange={ev => onSeek(Number(ev.target.value))}
        aria-label="Time"
      />
    </div>
  )
}

function markClass(ev: RecEvent, focus: number | null): string {
  if (ev.k === 'kill' && ev.by === focus && focus != null) return 'kill'
  if ((ev.k === 'kill' || ev.k === 'destroyed') && ev.o === focus && focus != null) return 'death'
  if (ev.k === 'kill' || ev.k === 'destroyed') return 'kill'
  if (ev.k === 'fired') return 'shot'
  return 'nav'
}

type Who = 'players' | 'ai' | 'all'

function FlightList({ meta, humans, t, focus, onPick }: {
  meta: RecMeta; humans: Set<number> | null; t: number; focus: number | null; onPick: (i: number) => void
}) {
  const [q, setQ] = useState('')
  const [onlyAirborne, setOnlyAirborne] = useState(false)
  const [whoPick, setWho] = useState<Who | null>(null)
  // Players by default once we know who they are (and there are any).
  const who: Who = whoPick ?? (humans && humans.size > 0 ? 'players' : 'all')
  const list = useMemo(() => {
    const s = q.trim().toLowerCase()
    return meta.flights
      .filter(f => who === 'all' || !humans || (who === 'players') === humans.has(f.i))
      .filter(f => !s || f.pilot.toLowerCase().includes(s) || f.aircraft.toLowerCase().includes(s))
      .sort((x, y) => (x.color ?? '').localeCompare(y.color ?? '') || x.t0 - y.t0)
  }, [meta, q, who, humans])
  const shown = onlyAirborne ? list.filter(f => t >= f.t0 && t <= f.t1) : list
  return (
    <>
      {humans && (
        <div className="rp-search" style={{ paddingBottom: 0 }}>
          {(['players', 'ai', 'all'] as Who[]).map(w => (
            <button key={w} className={`rp-chip ${who === w ? 'on' : ''}`} onClick={() => setWho(w)}>
              {w === 'players' ? `PLAYERS ${humans.size}` : w === 'ai' ? 'AI' : 'ALL'}
            </button>
          ))}
        </div>
      )}
      <div className="rp-search">
        <input placeholder="Pilot or aircraft…" value={q} onChange={ev => setQ(ev.target.value)} />
        <button className={`rp-chip ${onlyAirborne ? 'on' : ''}`} onClick={() => setOnlyAirborne(v => !v)} title="Only flights in the air now">NOW</button>
      </div>
      <div className="rp-list">
        {shown.length === 0 && <div className="rp-empty">No flights</div>}
        {shown.map(f => <FlightRowView key={f.i} f={f} live={t >= f.t0 && t <= f.t1} on={f.i === focus} onPick={onPick} />)}
      </div>
    </>
  )
}

function FlightRowView({ f, live, on, onPick }: { f: RecFlight; live: boolean; on: boolean; onPick: (i: number) => void }) {
  return (
    <button className={`rp-row ${on ? 'on' : ''} ${live ? '' : 'dim'}`} onClick={() => onPick(f.i)}>
      <span className="rp-dot" style={{ background: sideHex(f.color) }} />
      <span className="rp-row-main">
        <span className="rp-row-name">{f.pilot}</span>
        <span className="rp-row-sub">{f.aircraft.replace(/_/g, ' ')} · {fmtDuration(f.t1 - f.t0)}</span>
      </span>
      <span className="rp-row-stats">
        {f.kills > 0 && <span className="k">{f.kills}K</span>}
        {f.shots > 0 && <span title="shots / hits">{f.shots}S{f.hits != null ? `/${f.hits}H` : ''}</span>}
        {f.fate === 'destroyed' && <span className="x">✕</span>}
      </span>
    </button>
  )
}

function EventList({ meta, t, focus, onSeek }: { meta: RecMeta; t: number; focus: number | null; onSeek: (t: number) => void }) {
  const [mine, setMine] = useState(focus != null)
  const [shots, setShots] = useState(false)
  const name = (i?: number | null) => (i != null && meta.objects[i] ? objLabel(meta.objects[i]) : 'unknown')
  const wname = (i?: number | null) => (i != null && meta.objects[i] ? (meta.objects[i].n ?? 'weapon').replace(/_/g, ' ') : null)
  const list = useMemo(() => meta.events.filter(ev => {
    if (ev.k === 'fired' && !shots) return false
    if (mine && focus != null) return ev.o === focus || ev.by === focus
    return true
  }), [meta, mine, shots, focus])
  const text = (ev: RecEvent) => {
    switch (ev.k) {
      case 'fired': {
        const at = ev.tg != null ? ` at ${name(ev.tg)}` : ''
        const res = ev.hit ? ' · HIT' : ev.md != null ? ` · missed by ${ev.md >= 1852 ? `${(ev.md / 1852).toFixed(1)} nm` : `${ev.md} m`}` : ''
        return `${name(ev.o)} fired ${wname(ev.w) ?? ''}${at}${res}`
      }
      case 'kill': return ev.by != null
        ? `${name(ev.by)} destroyed ${name(ev.o)}${wname(ev.w) ? ` · ${wname(ev.w)}` : ''}`
        : `${name(ev.o)} destroyed by ${wname(ev.w)}`
      case 'destroyed': return `${name(ev.o)} destroyed`
      case 'lost': return `${name(ev.o)} lost in flight (guns, crash, or left the aircraft)`
      case 'takeoff': return `${name(ev.o)} took off`
      case 'landing': return `${name(ev.o)} landed`
    }
  }
  return (
    <>
      <div className="rp-search">
        {focus != null && <button className={`rp-chip ${mine ? 'on' : ''}`} onClick={() => setMine(v => !v)}>THIS FLIGHT</button>}
        <button className={`rp-chip ${shots ? 'on' : ''}`} onClick={() => setShots(v => !v)}>SHOTS</button>
      </div>
      <div className="rp-list">
        {list.length === 0 && <div className="rp-empty">No events</div>}
        {list.slice(0, 600).map((ev, i) => (
          <button key={i} className={`rp-ev rp-ev-${ev.k} ${ev.t <= t ? '' : 'dim'}`} onClick={() => onSeek(ev.t - 8_000)}>
            <span className="rp-ev-t">{fmtClock(meta.start_ms + ev.t)}</span>
            <span>{text(ev)}</span>
          </button>
        ))}
      </div>
    </>
  )
}

