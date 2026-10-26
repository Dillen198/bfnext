// The flight replay viewer, laid out like Tacview: the picture full-bleed,
// the object tree on the left, the selected object's telemetry on the right,
// the timeline with its event marks along the bottom.
//
// /replay/:rec plays a server recording, streamed in chunks; /replay/file
// plays a Tacview file from this computer, read whole in the browser.
// `?focus=<object index>` opens on one object and follows it; `?t=<ms>` opens
// at a moment (both are what the share button copies).

import { Fragment, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import QueryState from '../components/QueryState'
import { Link, useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import {
  Play, Pause, ChevronLeft, ChevronRight, ChevronsLeft, ChevronsRight, ChevronUp, ChevronDown, Link as LinkIcon, Eye, EyeOff, Download, Upload,
  Search, Info, X, Aircraft, Radar, KillFeed, Crosshair, Rankings, Bombing, Layers as LayersIcon, Compass, Maximize2, Activity,
  type IconComponent,
} from '@icons'
import { API_ROOT } from '../api'
import {
  TrackStore, fetchMeta, fetchHumans, fetchRangeHits, sideHex, objLabel, isAir, isWeapon, isEffect, isStepChannel,
  fmtDuration, fmtClock, gameClock, M_TO_FT, MS_TO_KT,
  type RecMeta, type RecEvent, type RecFlight, type RecObject, type State, type RangeHit, type TrackSource, type Kind,
} from './data'
import { ReplayEngine, type EngineUi, type ViewMode, type CamMode, type ViewOpts, type Basemap } from './engine'
import * as mgrs from 'mgrs'
import Charts from './Charts'
import Shots from './Shots'
import Stats from './Stats'
import { downloadCsv } from './csv'
import { type Units, fmtAlt, fmtSpeed, fmtVs, fmtWeight, fmtDist } from './units'
import { braa, aspectWord, gAt, nearest, machOf, iasOf, tasOf, flightStats, M_TO_NM } from './analysis'
import { bearingDeg, distM } from './data'
import { dcsXZ, theatreAt } from './dcs/theatre'
import { tacviewType } from '../../../shared/models/catalog'
import { acmiFrom, setPendingFile } from './local'
import './replay.css'

const SPEEDS = [0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128]

export default function ReplayPage() {
  const { rec = '' } = useParams<{ rec: string }>()
  const meta = useQuery({ queryKey: ['replay-meta', rec], queryFn: () => fetchMeta(rec), staleTime: Infinity, retry: 1 })
  const store = useMemo(() => (meta.data ? new TrackStore(rec, meta.data) : null), [rec, meta.data])
  if (meta.isLoading || meta.isError || !store) {
    return (
      <div className="rp-state">
        <QueryState isLoading={meta.isLoading} isError={meta.isError || !meta.data} error={meta.error ?? 'Recording not available'} what="recording" onRetry={() => meta.refetch()} />
        {!meta.isLoading && <Link to="/replay" className="rp-btn">All recordings</Link>}
      </div>
    )
  }
  return <Viewer source={store} rec={rec} />
}

type SideTab = 'flights' | 'objects' | 'events' | 'shots' | 'stats' | 'range'

const TABS: [SideTab, string, IconComponent][] = [
  ['flights', 'Flights', Aircraft],
  ['objects', 'Objects', Radar],
  ['events', 'Events', KillFeed],
  ['shots', 'Shots', Crosshair],
  ['stats', 'Stats', Rankings],
  ['range', 'Range', Bombing],
]

// Per-viewer conveniences; storage may be unavailable (private windows).
function stored(key: string, dflt: boolean): boolean {
  try { const v = localStorage.getItem(key); return v == null ? dflt : v === '1' } catch { return dflt }
}
function store(key: string, v: boolean) {
  try { localStorage.setItem(key, v ? '1' : '0') } catch { /* no storage */ }
}

/** The viewer over any track source. `rec` is the server recording id
 *  (null for a local file: no pilot lookup, no download, no range results). */
export function Viewer({ source, rec, fileName }: { source: TrackSource; rec: string | null; fileName?: string }) {
  const meta = source.meta
  const navigate = useNavigate()
  const [params, setParams] = useSearchParams()
  const rootEl = useRef<HTMLDivElement>(null)
  const mapEl = useRef<HTMLDivElement>(null)
  const engine = useRef<ReplayEngine | null>(null)
  const coordsEl = useRef<HTMLDivElement>(null)
  const [ui, setUi] = useState<EngineUi | null>(null)
  const [side, setSide] = useState<SideTab>('flights')
  const [leftOpen, setLeftOpen] = useState(() => window.innerWidth > 900)
  const [layersOpen, setLayersOpen] = useState(false)
  const [help, setHelp] = useState(false)
  const [fit, setFit] = useState(true)
  const [copied, setCopied] = useState(false)
  const [dropping, setDropping] = useState(false)
  const [charts, setCharts] = useState(() => stored('rp-charts', false))
  const [cardOpen, setCardOpen] = useState(() => window.innerWidth > 900)
  const [hint, setHint] = useState(() => !stored('rp-hint-seen', false))
  const [full, setFull] = useState(false)
  const [narrow, setNarrow] = useState(() => window.innerWidth <= 900)
  // Which flights players flew; null (show everything) on an older bfdb or a local file.
  const humansQ = useQuery({ queryKey: ['replay-humans', rec], queryFn: () => fetchHumans(rec!), staleTime: 300_000, retry: false, enabled: !!rec })
  const humans = useMemo(() => (humansQ.data ? new Set(humansQ.data) : null), [humansQ.data])
  // Graded training-range results in this recording: a recording of the range server has them
  const rangeQ = useQuery({ queryKey: ['replay-range', rec], queryFn: () => fetchRangeHits(rec!), staleTime: 300_000, retry: false, enabled: !!rec })
  const rangeHits = rangeQ.data?.items ?? []

  // Engine lifetime = this source.
  useEffect(() => {
    if (!mapEl.current) return
    const focus = params.get('focus')
    const fi = focus != null && meta.objects[Number(focus)] ? Number(focus) : null
    const t0 = params.get('t') != null ? Number(params.get('t')) : fi != null ? meta.objects[fi].t0 : 0
    const e = new ReplayEngine(mapEl.current, source, 0, meta.duration_ms, t0)
    engine.current = e
    e.onUi = setUi
    e.onCursor = ll => {
      const el = coordsEl.current
      if (!el) return
      el.textContent = ll ? coordText(ll) : ''
    }
    if (fi != null) e.setFocus(fi)
    e.kick()
    return () => { e.destroy(); engine.current = null }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [source])

  // Follow/centre within the part of the map the panels leave visible.
  const focus = ui?.focus ?? null
  const wideSide = side === 'stats'
  useEffect(() => {
    const apply = () => {
      const wide = window.innerWidth > 900
      const left = !wide ? 0 : leftOpen ? (wideSide ? 470 : 340) : 56
      engine.current?.setPadding({ top: 60, right: wide && focus != null ? 320 : 0, bottom: charts && wide && focus != null ? 230 : 110, left })
    }
    apply()
    window.addEventListener('resize', apply)
    return () => window.removeEventListener('resize', apply)
  }, [leftOpen, wideSide, focus, charts, source])

  useEffect(() => {
    const on = () => setNarrow(window.innerWidth <= 900)
    window.addEventListener('resize', on)
    return () => window.removeEventListener('resize', on)
  }, [])

  // Fullscreen is the viewer alone, without the dashboard round it.
  useEffect(() => {
    const on = () => setFull(document.fullscreenElement === rootEl.current)
    document.addEventListener('fullscreenchange', on)
    return () => document.removeEventListener('fullscreenchange', on)
  }, [])

  // The first-run hint goes once someone has picked something to follow.
  useEffect(() => { if (focus != null && hint) { setHint(false); store('rp-hint-seen', true) } }, [focus, hint])

  // Keyboard, Tacview-ish. `?` lists them.
  useEffect(() => {
    const onKey = (ev: KeyboardEvent) => {
      const e = engine.current
      if (!e || ev.ctrlKey || ev.metaKey || ev.altKey || (ev.target instanceof Element && ev.target.closest('input, select, textarea'))) return
      const cams: Record<string, CamMode> = { Digit1: 'follow', Digit2: 'chase', Digit3: 'cockpit', Digit4: 'satellite', Digit5: 'padlock', Digit0: 'free' }
      if (ev.code === 'Space') { ev.preventDefault(); e.toggle() }
      else if (ev.code === 'ArrowRight') { ev.preventDefault(); e.setTime(e.t + (ev.shiftKey ? 60_000 : 10_000)) }
      else if (ev.code === 'ArrowLeft') { ev.preventDefault(); e.setTime(e.t - (ev.shiftKey ? 60_000 : 10_000)) }
      else if (ev.code === 'Period') e.step(ev.shiftKey ? 1_000 : 100)
      else if (ev.code === 'Comma') e.step(ev.shiftKey ? -1_000 : -100)
      else if (ev.code === 'BracketRight') e.setSpeed(SPEEDS[Math.min(SPEEDS.length - 1, SPEEDS.indexOf(e.speed) + 1)] ?? 1)
      else if (ev.code === 'BracketLeft') e.setSpeed(SPEEDS[Math.max(0, SPEEDS.indexOf(e.speed) - 1)] ?? 1)
      else if (ev.code === 'KeyM') e.setMode(e.mode === '2d' ? '3d' : '2d')
      else if (ev.code === 'KeyL') e.setOpts({ labels: !e.opts.labels })
      else if (ev.code === 'KeyT') e.setOpts({ trail: e.opts.trail > 0 ? 0 : 60_000 })
      else if (ev.code === 'KeyR') e.setOpts({ ribbons: !e.opts.ribbons })
      else if (ev.code === 'KeyG') e.setOpts({ threats: !e.opts.threats })
      else if (ev.code === 'KeyB') e.startMeasuring(!e.measuring)
      else if (ev.code === 'KeyV') e.setOpts({ vectors: !e.opts.vectors })
      else if (ev.code === 'KeyD') e.setRuling(!e.ruling)
      else if (ev.code === 'KeyC') setCharts(c => { store('rp-charts', !c); return !c })
      else if (ev.code === 'KeyF') toggleFullscreen(rootEl.current)
      else if (ev.code === 'PageDown') { ev.preventDefault(); e.jumpEvent(1) }
      else if (ev.code === 'PageUp') { ev.preventDefault(); e.jumpEvent(-1) }
      else if (ev.code === 'Escape') { if (e.ruling) e.setRuling(false); else if (e.measure != null || e.measuring) e.setMeasure(null); else e.setFocus(null) }
      else if (ev.code === 'Tab') { ev.preventDefault(); cycleAircraft(e, ev.shiftKey ? -1 : 1) }
      else if (ev.key === '?') setHelp(h => !h)
      else if (cams[ev.code]) { if (cams[ev.code] !== 'follow' && e.mode === '2d') e.setMode('3d'); e.setCam(cams[ev.code]) }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])

  const fo = focus != null ? meta.objects[focus] : null

  // Keep ?focus= in the URL so a reload or a copied link lands on the object.
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

  const openFile = (f: File) => {
    setPendingFile(f)
    navigate('/replay/file')
  }

  // Following something that is not in the picture now jumps to when it is.
  const follow = (i: number) => {
    const ee = engine.current, o = meta.objects[i]
    if (!ee || !o) return
    ee.setFocus(i)
    if (ee.t < o.t0 || ee.t > o.t1) ee.setTime(o.t0)
    if (!fit) setFit(true)
    if (narrow) setLeftOpen(false) // a phone shows the map, not the list
  }
  const toggleCharts = () => setCharts(c => { store('rp-charts', !c); return !c })
  const openTab = (k: SideTab) => { setSide(k); setLeftOpen(true) }

  const e = engine.current
  const game = gameClock(meta, t)
  const mode = ui?.mode ?? '2d'
  const units: Units = ui?.opts.units ?? 'imperial'
  const showCharts = charts && !narrow && !!fo && !isEffect(fo.k)
  const tabs = TABS.filter(([k]) => k !== 'range' || rangeHits.length > 0)
  const date = meta.start_ms ? new Date(meta.start_ms).toISOString().slice(0, 10) : 'Local file'

  return (
    <div
      ref={rootEl}
      className={`rp-root ${dropping ? 'rp-dropping' : ''} ${full ? 'rp-full' : ''} ${showCharts ? 'rp-charts-open' : ''}`}
      onDragOver={ev => { if (ev.dataTransfer.types.includes('Files')) { ev.preventDefault(); setDropping(true) } }}
      onDragLeave={ev => { if (ev.currentTarget === ev.target) setDropping(false) }}
      onDrop={ev => { ev.preventDefault(); setDropping(false); const f = acmiFrom(ev.dataTransfer); if (f) openFile(f) }}
    >
      <div ref={mapEl} className="rp-map" />

      {/* ── top bar: what this is, how to look at it, what to do with it ── */}
      <header className="rp-top">
        <Link to="/replay" className="rp-icon-btn" title="All recordings"><ChevronLeft size={16} /></Link>
        <div className="rp-title">
          <div className="rp-title-main" title={meta.title || fileName}>{meta.title || fileName || 'Recording'}</div>
          <div className="rp-title-sub">
            {date} · {fmtDuration(meta.duration_ms)} · {meta.flights.length} flights · {meta.objects.length.toLocaleString()} objects
          </div>
        </div>
        {ui && (
          <div className="rp-counts" title="Objects in the picture now, by coalition">
            <span style={{ color: sideHex('Blue') }}><Aircraft size={13} /> {ui.counts.blue}</span>
            <span style={{ color: sideHex('Red') }}><Aircraft size={13} /> {ui.counts.red}</span>
            {ui.counts.other > 0 && <span style={{ color: sideHex(null) }}><Aircraft size={13} /> {ui.counts.other}</span>}
          </div>
        )}
        <div className="rp-tools">
          <div className="rp-seg rp-seg-view" role="group" aria-label="View">
            <button className={mode === '2d' ? 'on' : ''} onClick={() => e?.setMode('2d')} title="Map view (M)">Map</button>
            <button className={mode === '3d' ? 'on' : ''} onClick={() => e?.setMode('3d')} title="3D view (M)">3D</button>
          </div>
          <div className="rp-pop-anchor">
            <button className={`rp-tool ${layersOpen ? 'on' : ''}`} onClick={() => setLayersOpen(o => !o)} aria-expanded={layersOpen} title="What the picture shows">
              <LayersIcon size={15} /><span>Layers</span>
            </button>
            {layersOpen && ui && <Layers ui={ui} onSet={o => e?.setOpts(o)} onBasemap={b => e?.setBasemap(b)} onClose={() => setLayersOpen(false)} />}
          </div>
          {mode === '2d' && (
            <button className={`rp-tool rp-wide-only ${ui?.ruling ? 'on' : ''}`} onClick={() => e?.setRuling(!ui?.ruling)} title="Distance and bearing between two points on the map (D)">
              <Compass size={15} /><span>Ruler</span>
            </button>
          )}
          <span className="rp-sep" />
          <label className="rp-icon-btn rp-wide-only" title="Open a Tacview file from this computer (or drop one anywhere)">
            <Upload size={15} />
            <input type="file" accept=".acmi" hidden onChange={ev => { const f = ev.target.files?.[0]; if (f) openFile(f) }} />
          </label>
          {rec && (
            <a className="rp-icon-btn rp-wide-only" href={`${API_ROOT}/api/replay/rec/${encodeURIComponent(rec)}/acmi`} title="Download the original recording to open in Tacview">
              <Download size={15} />
            </a>
          )}
          {rec && (
            <button className="rp-icon-btn rp-wide-only" onClick={share} title="Copy a link to this moment">
              <LinkIcon size={15} />
              {copied && <span className="rp-toast">Link copied</span>}
            </button>
          )}
          <button className={`rp-icon-btn rp-wide-only ${full ? 'on' : ''}`} onClick={() => toggleFullscreen(rootEl.current)} title={full ? 'Leave full screen (F)' : 'Full screen (F)'}>
            <Maximize2 size={15} />
          </button>
          <button className={`rp-icon-btn ${help ? 'on' : ''}`} onClick={() => setHelp(h => !h)} title="Keys and tools (?)"><Info size={15} /></button>
        </div>
      </header>

      {/* ── left: what is in the recording ── */}
      {leftOpen ? (
        <aside className={`rp-side ${wideSide ? 'wide' : ''}`}>
          <div className="rp-tabs" role="tablist">
            {tabs.map(([k, label, Icon]) => (
              <button key={k} role="tab" aria-selected={side === k} className={side === k ? 'on' : ''} onClick={() => setSide(k)} title={label}>
                <Icon size={15} /><span>{label}</span>
              </button>
            ))}
            <button className="rp-tabs-hide" onClick={() => setLeftOpen(false)} title="Hide the panel"><ChevronsLeft size={14} /></button>
          </div>
          {side === 'flights' && <FlightList meta={meta} humans={humans} t={t} focus={focus} onPick={follow} />}
          {side === 'objects' && (
            <ObjectTree
              meta={meta} t={t} focus={focus} measure={ui?.measure ?? null}
              onPick={(i, shift) => { if (shift && focus != null && i !== focus) e?.setMeasure(i); else follow(i) }}
            />
          )}
          {side === 'shots' && (
            <Shots
              meta={meta} store={source} focus={focus} version={source.version}
              onPick={sh => {
                if (!e) return
                if (sh.ev.o != null && sh.ev.o !== focus) e.setFocus(sh.ev.o)
                e.setMeasure(sh.ev.tg ?? null)
                e.setTime(sh.ev.t - 5_000)
              }}
            />
          )}
          {side === 'events' && <EventList meta={meta} t={t} focus={focus} onSeek={x => e?.setTime(x)} />}
          {side === 'stats' && <Stats meta={meta} humans={humans} focus={focus} onPick={follow} />}
          {side === 'range' && (
            <RangeList
              meta={meta} hits={rangeHits} site={rangeQ.data?.site ?? ''} t={t}
              onSeek={(hit, flight) => { if (!e) return; if (flight != null) e.setFocus(flight); e.setTime(Math.max(0, hit.t - 25_000)) }}
            />
          )}
        </aside>
      ) : (
        <nav className="rp-rail" aria-label="Panels">
          {tabs.map(([k, label, Icon]) => (
            <button key={k} onClick={() => openTab(k)} title={label} aria-label={label}><Icon size={17} /></button>
          ))}
          <button className="rp-rail-open" onClick={() => setLeftOpen(true)} title="Show the panel"><ChevronsRight size={14} /></button>
        </nav>
      )}

      {/* ── right: the followed object ── */}
      {fo && focus != null && (
        <section className={`rp-focus ${cardOpen ? '' : 'closed'}`} aria-label="Selected object">
          <div className="rp-card-head">
            <span className="rp-dot" style={{ background: sideHex(fo.c) }} />
            <div className="rp-card-title">
              <div className="rp-focus-name">{objLabel(fo)}</div>
              <TypeLine o={fo} />
            </div>
            <button className="rp-icon-btn rp-x" onClick={() => setCardOpen(o => !o)} title={cardOpen ? 'Collapse' : 'Expand'}>
              {cardOpen ? <ChevronUp size={13} /> : <ChevronDown size={13} />}
            </button>
            <button className="rp-icon-btn rp-x" onClick={() => e?.setFocus(null)} title="Stop following (Esc)"><X size={13} /></button>
          </div>
          {cardOpen && (
            <div className="rp-card-body">
              <div className="rp-seg rp-cams" role="group" aria-label="Camera">
                {camModes(mode, ui?.measure != null).map(([c, label, title]) => (
                  <button key={c} className={ui?.cam === c ? 'on' : ''} onClick={() => e?.setCam(c)} title={title}>{label}</button>
                ))}
              </div>
              <Identity o={fo} />
              <Telemetry s={ui?.focusState ?? null} kind={fo.k} alive={t >= fo.t0 && t <= fo.t1} g={focusG(source, focus, t)} meta={meta} units={units} />
              <Bullseye source={source} side={fo.c} s={ui?.focusState ?? null} t={t} units={units} />
              {isAir(fo.k) && <FlightSummary meta={meta} store={source} idx={focus} version={source.version} />}
              <Measure
                meta={meta}
                a={ui?.focusState ?? null}
                b={ui?.measureState ?? null}
                measure={ui?.measure ?? null}
                measuring={ui?.measuring ?? false}
                onStart={() => e?.startMeasuring(!ui?.measuring)}
                onClear={() => e?.setMeasure(null)}
              />
              <Channels s={ui?.focusState ?? null} meta={meta} />
              <button
                className="rp-chip rp-wide-btn" title="The whole track and every recorded property, one row per sample"
                onClick={() => { const s = source.wholeTrack(focus); if (s) downloadCsv(meta, focus, s) }}
              >
                <Download size={11} /> Export telemetry (CSV)
              </button>
            </div>
          )}
        </section>
      )}

      {mode === '3d' && (
        <a className="rp-credit" href={`${import.meta.env.BASE_URL}models/CREDITS.txt`} target="_blank" rel="noreferrer">
          3D models: Tacview (GPL-3.0) -- credits · terrain: Mapterhorn, Mapzen/AWS · imagery: Esri
        </a>
      )}

      {/* ── bottom: charts + transport ── */}
      <div className="rp-bottom">
        {showCharts && focus != null && (
          <Charts store={source} idx={focus} range={range} t={t} onSeek={x => e?.setTime(x)} version={source.version} units={units} />
        )}
        <div className="rp-transport">
          <div className="rp-transport-btns">
            <button className="rp-icon-btn rp-wide-only" onClick={() => e?.jumpEvent(-1)} title="Previous kill, shot or bookmark (Page Up)" aria-label="Previous event"><ChevronsLeft size={15} /></button>
            <button className="rp-icon-btn rp-wide-only" onClick={() => e?.setTime(e.t - 10_000)} title="Back 10 s (←)" aria-label="Back 10 seconds"><ChevronLeft size={15} /></button>
            <button className="rp-play" onClick={() => e?.toggle()} aria-label={ui?.playing ? 'Pause' : 'Play'} title="Play / pause (Space)">
              {ui?.playing ? <Pause size={18} /> : <Play size={18} />}
            </button>
            <button className="rp-icon-btn rp-wide-only" onClick={() => e?.setTime(e.t + 10_000)} title="Forward 10 s (→)" aria-label="Forward 10 seconds"><ChevronRight size={15} /></button>
            <button className="rp-icon-btn rp-wide-only" onClick={() => e?.jumpEvent(1)} title="Next kill, shot or bookmark (Page Down)" aria-label="Next event"><ChevronsRight size={15} /></button>
          </div>
          <SpeedPicker speed={ui?.speed ?? 1} onSet={s => e?.setSpeed(s)} />
          <div className="rp-clock" title={game ? `Game time; ${fmtClock(meta.start_ms + t)} real` : 'Zulu'}>
            <div className="rp-clock-main">{game ?? fmtClock(meta.start_ms + t)}</div>
            <div className="rp-clock-sub">{fmtDuration(t - range[0])} / {fmtDuration(range[1] - range[0])}</div>
          </div>
          <Timeline meta={meta} range={range} t={t} focus={focus} span={!fit && fo ? [fo.t0, fo.t1] : null} onSeek={x => e?.setTime(x)} />
          {fo && (
            <div className="rp-seg" role="group" aria-label="Timeline span">
              <button className={fit ? 'on' : ''} onClick={() => setFit(true)} title="The timeline spans this flight">Flight</button>
              <button className={fit ? '' : 'on'} onClick={() => setFit(false)} title="The timeline spans the whole recording">All</button>
            </div>
          )}
          {fo && !isEffect(fo.k) && (
            <button className={`rp-tool rp-wide-only ${charts ? 'on' : ''}`} onClick={toggleCharts} title="Telemetry charts (C)">
              <Activity size={15} /><span>Charts</span>
            </button>
          )}
          {ui?.buffering && <span className="rp-buffer">loading…</span>}
        </div>
        {ui?.error && <div className="rp-error">{ui.error}</div>}
      </div>

      <div ref={coordsEl} className="rp-coords" />
      {ui?.ruler?.b && (
        <div className="rp-ruler">
          RULER {fmtDist(ui.ruler.dist, units)} · {String(Math.round(ui.ruler.brg) % 360).padStart(3, '0')}°
          <button className="rp-icon-btn rp-x" onClick={() => e?.setRuling(false)} title="Clear (Esc)"><X size={12} /></button>
        </div>
      )}
      {ui?.ruling && !ui.ruler?.b && <div className="rp-ruler">{ui.ruler ? 'Click the second point' : 'Click the first point'}</div>}
      {ui?.measuring && <div className="rp-ruler">Click the object to measure to · Esc cancels</div>}
      {hint && focus == null && (
        <div className="rp-hint">
          <span><b>Click any aircraft</b> to follow it, or pick one from <b>Flights</b>. <kbd>Space</kbd> plays, <kbd>?</kbd> lists every key.</span>
          <button className="rp-icon-btn rp-x" onClick={() => { setHint(false); store('rp-hint-seen', true) }} title="Got it"><X size={12} /></button>
        </div>
      )}
      {help && <Help onClose={() => setHelp(false)} />}
      {dropping && <div className="rp-drop">Drop a Tacview file to open it</div>}
    </div>
  )
}

function toggleFullscreen(el: HTMLElement | null) {
  if (document.fullscreenElement) document.exitFullscreen().catch(() => {})
  else el?.requestFullscreen?.().catch(() => {})
}

/** Playback speed: the common ones as buttons, the rest a step away. */
function SpeedPicker({ speed, onSet }: { speed: number; onSet: (s: number) => void }) {
  const i = SPEEDS.indexOf(speed)
  return (
    <div className="rp-speed" role="group" aria-label="Speed">
      <button onClick={() => onSet(SPEEDS[Math.max(0, i - 1)] ?? 1)} disabled={i <= 0} title="Slower ([)" aria-label="Slower">−</button>
      <span title="Playback speed">{speed < 1 ? `${speed}` : speed}×</span>
      <button onClick={() => onSet(SPEEDS[Math.min(SPEEDS.length - 1, i + 1)] ?? 1)} disabled={i >= SPEEDS.length - 1} title="Faster (])" aria-label="Faster">+</button>
    </div>
  )
}

/** Tab / Shift+Tab: the next aircraft in the air, by name. */
function cycleAircraft(e: ReplayEngine, dir: 1 | -1) {
  const { objects } = e.store.meta
  const live = e.store.present(e.t)
    .filter(i => objects[i] && isAir(objects[i].k) && e.t >= objects[i].t0 && e.t <= objects[i].t1)
    .sort((a, b) => objLabel(objects[a]).localeCompare(objLabel(objects[b])))
  if (!live.length) return
  const at = e.focus != null ? live.indexOf(e.focus) : -1
  e.setFocus(live[(at + dir + live.length) % live.length])
}

/** The camera buttons a view offers: [mode, label, tooltip]. */
function camModes(mode: ViewMode, measuring: boolean): [CamMode, string, string][] {
  if (mode === '2d') return [['follow', 'Follow', 'Keep the object centred (1)'], ['free', 'Free', 'Pan the map yourself (0)']]
  const m: [CamMode, string, string][] = [
    ['follow', 'External', 'Orbit the object: drag to look round, wheel to zoom (1)'],
    ['chase', 'Chase', 'Behind the aircraft, wheel for distance (2)'],
    ['cockpit', 'Cockpit', 'From the pilot seat (3)'],
    ['satellite', 'Top', 'Straight down from above, wheel for height (4)'],
  ]
  if (measuring) m.push(['padlock', 'Padlock', 'Behind the aircraft, looking at the target (5)'])
  m.push(['free', 'Free', 'Fly the camera yourself (0)'])
  return m
}

type Toggle = Exclude<keyof ViewOpts, 'sides' | 'scale' | 'units' | 'labelDetail'>
type LayerRow = [Toggle, string, string, ('2d' | '3d')?]
const LAYER_GROUPS: [string, LayerRow[]][] = [
  ['Display', [
    ['labels', 'Labels', 'L'],
    ['trail', 'Trails', 'T'],
    ['ribbons', 'Ribbons', 'R', '3d'],
    ['vectors', 'Velocity vectors', 'V'],
    ['verticals', 'Altitude lines', '', '3d'],
    ['hud', 'Cockpit HUD', '', '3d'],
    ['grid', 'Lat/lon grid', '', '2d'],
    ['airfields', 'Airfields', '', '2d'],
  ]],
  ['Objects', [
    ['weapons', 'Weapons', ''],
    ['ground', 'Ground & sea', ''],
    ['effects', 'Flares, chaff, tracers', ''],
  ]],
  ['Tactical', [
    ['locks', 'Radar locks', ''],
    ['radar', 'Radar sweeps', ''],
    ['threats', 'Threat rings', 'G'],
    ['bullseye', 'Bullseye', ''],
  ]],
]
const BASEMAPS: [Basemap, string, string][] = [
  ['dark', 'Dark', 'A dark map that keeps the tracks readable'],
  ['sat', 'Satellite', 'Satellite imagery'],
  ['topo', 'Topo', 'Topographic map in the style of the DCS F10 map'],
]

function Layers({ ui, onSet, onBasemap, onClose }: {
  ui: EngineUi; onSet: (o: Partial<ViewOpts>) => void; onBasemap: (b: Basemap) => void; onClose: () => void
}) {
  useEffect(() => {
    const close = (ev: MouseEvent) => { if (!(ev.target as HTMLElement).closest('.rp-pop-anchor')) onClose() }
    const esc = (ev: KeyboardEvent) => { if (ev.code === 'Escape') { ev.stopPropagation(); onClose() } }
    window.addEventListener('mousedown', close)
    window.addEventListener('keydown', esc, true)
    return () => { window.removeEventListener('mousedown', close); window.removeEventListener('keydown', esc, true) }
  }, [onClose])
  return (
    <div className="rp-pop" role="dialog" aria-label="Layers">
      {ui.mode === '2d' && (
        <div className="rp-pop-group">
          <div className="rp-pop-h">Map</div>
          <div className="rp-seg rp-seg-fill">
            {BASEMAPS.map(([b, label, title]) => (
              <button key={b} className={ui.basemap === b ? 'on' : ''} onClick={() => onBasemap(b)} title={title}>{label}</button>
            ))}
          </div>
        </div>
      )}
      {LAYER_GROUPS.map(([group, rows]) => {
        const shown = rows.filter(([, , , only]) => !only || only === ui.mode)
        return (
          <div key={group} className="rp-pop-group">
            <div className="rp-pop-h">{group}</div>
            <div className="rp-pop-grid">
              {shown.map(([k, label, key]) => {
                const on = k === 'trail' ? ui.opts.trail > 0 : !!ui.opts[k]
                return (
                  <button key={k} className={`rp-pop-row ${on ? 'on' : ''}`} aria-pressed={on} onClick={() => onSet(k === 'trail' ? { trail: on ? 0 : 60_000 } : { [k]: !on })}>
                    {on ? <Eye size={12} /> : <EyeOff size={12} />}
                    <span>{label}</span>
                    {key && <kbd>{key}</kbd>}
                  </button>
                )
              })}
            </div>
            {trailSlider(group, ui, onSet)}
          </div>
        )
      })}
      <div className="rp-pop-group">
        <div className="rp-pop-h">Show</div>
        <div className="rp-pop-line">
          <span>Sides</span>
          {(['blue', 'red', 'other'] as const).map(k => (
            <button key={k} className={`rp-chip ${ui.opts.sides[k] ? 'on' : ''}`} onClick={() => onSet({ sides: { ...ui.opts.sides, [k]: !ui.opts.sides[k] } })}
              style={ui.opts.sides[k] ? { color: sideHex(k === 'blue' ? 'Blue' : k === 'red' ? 'Red' : null) } : undefined}>
              {k === 'blue' ? 'Blue' : k === 'red' ? 'Red' : 'Neutral'}
            </button>
          ))}
        </div>
        <div className="rp-pop-line">
          <span>Labels</span>
          {(['min', 'std', 'full'] as const).map(k => (
            <button key={k} className={`rp-chip ${ui.opts.labelDetail === k ? 'on' : ''}`} onClick={() => onSet({ labelDetail: k })}>
              {k === 'min' ? 'Name' : k === 'std' ? 'Standard' : 'Full'}
            </button>
          ))}
        </div>
        <div className="rp-pop-line">
          <span>Units</span>
          {(['imperial', 'metric'] as const).map(k => (
            <button key={k} className={`rp-chip ${ui.opts.units === k ? 'on' : ''}`} onClick={() => onSet({ units: k })}>{k === 'imperial' ? 'ft · kt' : 'm · km/h'}</button>
          ))}
        </div>
        <div className="rp-pop-line">
          <span>Size</span>
          <input type="range" min={1} max={4} step={0.25} value={ui.opts.scale} onChange={ev => onSet({ scale: Number(ev.target.value) })} aria-label="Object size" />
          <b>{ui.opts.scale}×</b>
        </div>
      </div>
    </div>
  )
}

/** The trail length slider, under the Display group while trails are on. */
function trailSlider(group: string, ui: EngineUi, onSet: (o: Partial<ViewOpts>) => void) {
  if (group !== 'Display' || ui.opts.trail <= 0) return null
  return (
    <div className="rp-pop-line">
      <span>Trail</span>
      <input
        type="range" min={0} max={100} step={1} value={trailToSlider(ui.opts.trail)}
        onChange={ev => onSet({ trail: sliderToTrail(Number(ev.target.value)) })}
        aria-label="Trail length"
      />
      <b>{fmtTrail(ui.opts.trail)}</b>
    </div>
  )
}

// Trail length on a log scale: 5 s at the left, 30 min at the right, so the
// short trails a dogfight wants get as much of the slider as long ones.
const TRAIL_MIN = 5_000, TRAIL_MAX = 1_800_000
function sliderToTrail(v: number): number {
  const ms = TRAIL_MIN * (TRAIL_MAX / TRAIL_MIN) ** (v / 100)
  const step = ms < 60_000 ? 1_000 : ms < 600_000 ? 5_000 : 30_000
  return Math.round(ms / step) * step
}
function trailToSlider(ms: number): number {
  return Math.round((100 * Math.log(Math.max(TRAIL_MIN, Math.min(TRAIL_MAX, ms)) / TRAIL_MIN)) / Math.log(TRAIL_MAX / TRAIL_MIN))
}
function fmtTrail(ms: number): string {
  const s = Math.round(ms / 1000)
  if (s < 60) return `${s}s`
  const m = Math.floor(s / 60), r = s % 60
  return r ? `${m}m${String(r).padStart(2, '0')}` : `${m}m`
}

function Help({ onClose }: { onClose: () => void }) {
  const groups: [string, [string, string][]][] = [
    ['Playback', [
      ['Space', 'play / pause'],
      ['← →', '10 s back / forward (Shift: 1 min)'],
      [', .', 'step 0.1 s (Shift: 1 s)'],
      ['[ ]', 'slower / faster'],
      ['PgUp PgDn', 'previous / next kill, shot or bookmark'],
    ]],
    ['Following', [
      ['Tab', 'next aircraft in the air (Shift: previous)'],
      ['1 2 3 4 5 0', 'external, chase, cockpit, top, padlock, free camera'],
      ['B', 'measure to another object (or Shift-click it)'],
      ['Esc', 'stop measuring, then stop following'],
    ]],
    ['View', [
      ['M', 'map / 3D'],
      ['L T R G V', 'labels, trails, ribbons, threat rings, velocity vectors'],
      ['D', 'ruler: distance and bearing between two map points'],
      ['C', 'telemetry charts'],
      ['F', 'full screen'],
    ]],
  ]
  return (
    <div className="rp-help" role="dialog" aria-label="Keys and tools">
      <div className="rp-focus-head">
        <span className="rp-focus-name">Keys and tools</span>
        <button className="rp-icon-btn rp-x" onClick={onClose} title="Close (?)"><X size={13} /></button>
      </div>
      <div className="rp-help-cols">
        {groups.map(([g, rows]) => (
          <div key={g}>
            <div className="rp-card-h">{g}</div>
            <table>
              <tbody>{rows.map(([k, v]) => <tr key={k}><td><kbd>{k}</kbd></td><td>{v}</td></tr>)}</tbody>
            </table>
          </div>
        ))}
      </div>
      <p>
        Click anything on the map to follow it; Shift-click a second object for range, bearing, aspect and closure.
        Click a mark on the timeline to jump to just before it. Drop a Tacview <code>.acmi</code> file anywhere to
        watch it here -- it is read in your browser and never uploaded.
      </p>
    </div>
  )
}

/** G at t from the followed aircraft's whole track, once it has arrived. */
function focusG(store: TrackSource, idx: number, t: number): number | null {
  const s = store.wholeTrack(idx)
  if (!s) return null
  return gAt(s, nearest(s, t))
}

/** What it is: Tacview's long name for the type, else the recorded one. */
function TypeLine({ o }: { o: RecObject }) {
  const tv = tacviewType(o.n)
  const text = tv?.long ? `${tv.long}${o.n && tv.short !== o.n ? ` · ${o.n}` : ''}` : (o.n ?? o.k).replace(/_/g, ' ')
  return <div className="rp-focus-sub rp-long" title={text}>{text}</div>
}

function Identity({ o }: { o: RecObject }) {
  const bits = [o.g, o.cs && o.cs !== o.p ? o.cs : null, o.c, o.co].filter(Boolean)
  return (
    <>
      {bits.length > 0 && <div className="rp-focus-sub">{bits.join(' · ')}</div>}
      {o.ty && <div className="rp-focus-tags">{o.ty.split('+').map(x => <span key={x}>{x}</span>)}</div>}
    </>
  )
}

function Telemetry({ s, kind, alive, g, meta, units }: { s: State | null; kind: Kind; alive: boolean; g: number | null; meta: RecMeta; units: Units }) {
  if (!alive) return <div className="rp-card-note">Not in the picture at this moment</div>
  if (!s) return <div className="rp-card-note">Loading…</div>
  // DCS records attitude and AGL; IAS, TAS and Mach are worked out from the
  // track (marked ~) when the recording does not carry them.
  const ch = s.ch ?? {}
  const tas = ch.TAS ?? null
  const cells: [string, string][] = [
    ['ALT', fmtAlt(s.alt, units)],
    ['AGL', s.agl != null ? fmtAlt(s.agl, units) : '—'],
    ['VS', fmtVs(s.vs, units)],
  ]
  if (isAir(kind) || isWeapon(kind)) {
    cells.push(
      [s.ias != null ? 'IAS' : 'IAS ~', fmtSpeed(iasOf(s), units)],
      [tas != null ? 'TAS' : 'TAS ~', fmtSpeed(tas ?? tasOf(s), units)],
      [s.mach != null ? 'MACH' : 'MACH ~', machOf(s).toFixed(2)],
    )
  }
  cells.push(
    ['GS', fmtSpeed(s.gs, units)],
    ['HDG', `${String(Math.round(s.hdg) % 360).padStart(3, '0')}°`],
  )
  if (isAir(kind)) {
    cells.push(
      ['G', ch.VerticalGForce != null ? ch.VerticalGForce.toFixed(1) : g != null ? g.toFixed(1) : '—'],
      ['PITCH', `${Math.round(s.pitch)}°`],
      ['BANK', `${Math.round(s.roll)}°`],
      s.aoa != null ? ['AOA', `${s.aoa.toFixed(1)}°`] : ['AOA', '—'],
    )
    if (ch.FuelWeight != null) cells.push(['FUEL', fmtWeight(ch.FuelWeight, units)])
    if (ch.Throttle != null) cells.push(['THR', `${Math.round(ch.Throttle * 100)}%`])
  }
  const locks = ['LockedTarget', 'LockedTarget2', 'LockedTarget3'].map(k => ch[k]).filter((v): v is number => v != null && v >= 0)
  return (
    <>
      <div className="rp-card-h">Telemetry</div>
      <div className="rp-telemetry">
        {cells.map(([k, v], i) => (
          <div key={i}><span>{k}</span><b>{v}</b></div>
        ))}
      </div>
      {locks.length > 0 && (
        <div className="rp-focus-sub rp-lock">LOCK: {locks.map(i => (meta.objects[i] ? objLabel(meta.objects[i]) : '?')).join(', ')}</div>
      )}
    </>
  )
}

/** Bearing and range from the object's own coalition's bullseye, as a
 *  controller would call it ("bullseye 045 for 32"). */
function Bullseye({ source, side, s, t, units }: { source: TrackSource; side?: string | null; s: State | null; t: number; units: Units }) {
  const bull = useMemo(() => {
    const objs = source.meta.objects
    const i = objs.findIndex(o => o.k === 'bullseye' && o.c === side)
    return i >= 0 ? i : null
  }, [source, side])
  if (bull == null || !s) return null
  const b = source.state(bull, t)
  if (!b) return null
  const brg = Math.round(bearingDeg(b.lon, b.lat, s.lon, s.lat)) % 360
  return <div className="rp-focus-sub rp-bull">BULLSEYE {String(brg).padStart(3, '0')}° / {fmtDist(distM(b.lon, b.lat, s.lon, s.lat), units)}</div>
}

/** Cursor position: degrees-minutes and MGRS. */
function coordText([lon, lat]: [number, number]): string {
  const dm = (v: number, pos: string, neg: string, w: number) => {
    const a = Math.abs(v), d = Math.floor(a), m = (a - d) * 60
    return `${v >= 0 ? pos : neg}${String(d).padStart(w, '0')}°${m.toFixed(3).padStart(6, '0')}'`
  }
  let grid = ''
  try { grid = mgrs.forward([lon, lat], 5).replace(/^(\d+[A-Z])([A-Z]{2})(\d{5})(\d{5})$/, '$1 $2 $3 $4') } catch { /* outside MGRS */ }
  const th = theatreAt(lat, lon)
  let dcs = ''
  if (th) {
    const { x, z } = dcsXZ(lat, lon, th)
    dcs = `  ${th.name.toUpperCase()} X ${Math.round(x).toLocaleString()} Z ${Math.round(z).toLocaleString()}`
  }
  return `${dm(lat, 'N', 'S', 2)}  ${dm(lon, 'E', 'W', 3)}  ${grid}${dcs}`
}

/** Every other property the recording carries for this object, raw. */
function Channels({ s, meta }: { s: State | null; meta: RecMeta }) {
  const [open, setOpen] = useState(false)
  const ch = s?.ch
  const keys = ch ? Object.keys(ch).sort() : []
  if (!keys.length) return null
  return (
    <div className="rp-channels">
      <button className="rp-card-h rp-card-toggle" onClick={() => setOpen(o => !o)} aria-expanded={open}>
        {open ? <ChevronDown size={12} /> : <ChevronRight size={12} />} All recorded properties <span>{keys.length}</span>
      </button>
      {open && ch && (
        <table>
          <tbody>
            {keys.map(k => {
              const v = ch[k]
              const lock = /^LockedTarget\d?$/.test(k)
              const text = lock ? (v >= 0 && meta.objects[v] ? objLabel(meta.objects[v]) : 'none') : isStepChannel(k) ? String(Math.round(v)) : fmtNum(v)
              return <tr key={k}><td>{k}</td><td>{text}</td></tr>
            })}
          </tbody>
        </table>
      )}
    </div>
  )
}

const fmtNum = (v: number) => (Math.abs(v) >= 100 ? Math.round(v).toLocaleString() : Math.abs(v) >= 1 ? v.toFixed(1) : v.toFixed(3))

function FlightSummary({ meta, store, idx, version }: { meta: RecMeta; store: TrackSource; idx: number; version: number }) {
  const fl = meta.flights.find(f => f.i === idx)
  const st = useMemo(() => {
    const s = store.wholeTrack(idx)
    return s ? flightStats(s) : null
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [store, idx, version])
  return (
    <div className="rp-summary">
      <div className="rp-card-h">
        This flight
        {fl?.fate === 'destroyed' && <span className="rp-badge x">Lost</span>}
        {fl?.fate === 'landed' && <span className="rp-badge">Landed</span>}
      </div>
      <div className="rp-tiles">
        <div><b>{fl?.shots ?? 0}</b><span>Shots</span></div>
        <div><b>{fl?.hits ?? '—'}</b><span>Hits</span></div>
        <div><b className="k">{fl?.kills ?? 0}</b><span>Kills</span></div>
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
      <button className={`rp-chip rp-wide-btn ${measuring ? 'on' : ''}`} onClick={onStart}
        title="Range, bearing, closure and aspect to another object (B, or Shift-click it)">
        <Crosshair size={11} /> {measuring ? 'Click a target… (Esc cancels)' : 'Measure to another object'}
      </button>
    )
  }
  const o = meta.objects[measure]
  const r = a && b ? braa(a, b) : null
  return (
    <div className="rp-braa">
      <div className="rp-card-h">Measuring to</div>
      <div className="rp-focus-head">
        <span className="rp-dot" style={{ background: sideHex(o?.c) }} />
        <span className="rp-focus-name">{o ? objLabel(o) : '?'}</span>
        <button className="rp-icon-btn rp-x" onClick={onClear} title="Stop measuring (Esc)"><X size={13} /></button>
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
      ) : <div className="rp-card-note">Not both in the picture at this moment</div>}
    </div>
  )
}

/** Lead before an event when jumping to it, so it is seen happening. */
const EVENT_LEAD = 5_000

/**
 * The timeline: drag anywhere to scrub; hover for the time (and the event
 * under the pointer); click an event mark to jump to just before it. With the
 * whole recording shown, the followed flight's span is shaded.
 */
function Timeline({ meta, range, t, focus, span, onSeek }: {
  meta: RecMeta; range: [number, number]; t: number; focus: number | null; span: [number, number] | null; onSeek: (t: number) => void
}) {
  const [a, b] = range
  const len = Math.max(1, b - a)
  const el = useRef<HTMLDivElement>(null)
  const drag = useRef(false)
  const [hover, setHover] = useState<{ t: number; ev: RecEvent | null } | null>(null)
  const marks = useMemo(() => meta.events.filter(ev => {
    if (ev.t < a || ev.t > b) return false
    if (ev.k === 'bookmark') return true
    if (focus == null) return ev.k === 'kill' || ev.k === 'destroyed'
    return ev.o === focus || ev.by === focus || ev.w === focus
  }).slice(0, 400), [meta, a, b, focus])
  const pct = (x: number) => (Math.min(Math.max(x, a), b) - a) / len * 100
  // the time under the pointer, and the mark within a few pixels of it
  const at = (clientX: number) => {
    const r = el.current!.getBoundingClientRect()
    const x = a + Math.min(1, Math.max(0, (clientX - r.left) / r.width)) * len
    const tol = (6 / Math.max(1, r.width)) * len
    let best: RecEvent | null = null
    for (const ev of marks) if (Math.abs(ev.t - x) <= tol && (!best || Math.abs(ev.t - x) < Math.abs(best.t - x))) best = ev
    return { t: x, ev: best }
  }
  const tip = hover ? (hover.ev ? hover.ev.t : hover.t) : null
  return (
    <div
      ref={el}
      className="rp-timeline"
      role="slider" tabIndex={0} aria-label="Time"
      aria-valuemin={a} aria-valuemax={b} aria-valuenow={Math.round(t)} aria-valuetext={fmtDuration(t - a)}
      onPointerDown={ev => {
        if (ev.button !== 0) return
        el.current?.setPointerCapture(ev.pointerId)
        drag.current = true
        const h = at(ev.clientX)
        onSeek(h.ev ? Math.max(a, h.ev.t - EVENT_LEAD) : h.t)
      }}
      onPointerMove={ev => {
        const h = at(ev.clientX)
        setHover(h)
        if (drag.current && ev.buttons & 1) onSeek(h.t)
      }}
      onPointerUp={ev => { drag.current = false; el.current?.releasePointerCapture(ev.pointerId) }}
      onPointerLeave={() => { if (!drag.current) setHover(null) }}
    >
      <div className="rp-tl-track">
        {span && <div className="rp-tl-span" style={{ left: `${pct(span[0])}%`, width: `${pct(span[1]) - pct(span[0])}%` }} />}
        <div className="rp-tl-fill" style={{ width: `${pct(t)}%` }} />
      </div>
      {marks.map((ev, i) => (
        <span key={i} className={`rp-mark rp-mark-${markClass(ev, focus)} ${hover?.ev === ev ? 'hot' : ''}`} style={{ left: `${pct(ev.t)}%` }} />
      ))}
      <div className="rp-tl-thumb" style={{ left: `${pct(t)}%` }} />
      {hover && tip != null && (
        <div className="rp-tl-tip" style={{ left: `${pct(tip)}%` }}>
          <b>{gameClock(meta, tip) ?? fmtClock(meta.start_ms + tip)}</b>
          {hover.ev && <span>{eventText(meta, hover.ev)}</span>}
        </div>
      )}
    </div>
  )
}

/** One line for an event, as the Events list and the timeline show it. */
function eventText(meta: RecMeta, ev: RecEvent): string {
  const name = (i?: number | null) => (i != null && meta.objects[i] ? objLabel(meta.objects[i]) : 'unknown')
  const wname = (i?: number | null) => (i != null && meta.objects[i] ? (meta.objects[i].n ?? 'weapon').replace(/_/g, ' ') : null)
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
    case 'hit': return `${name(ev.o)} hit`
    case 'left': return `${name(ev.o)} left the area`
    case 'message': return ev.o != null ? `${name(ev.o)}: ${ev.text ?? ''}` : ev.text ?? 'message'
    case 'bookmark': return `★ ${ev.text ?? 'bookmark'}`
  }
}

function markClass(ev: RecEvent, focus: number | null): string {
  if (ev.k === 'bookmark') return 'book'
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
              {w === 'players' ? `Players ${humans.size}` : w === 'ai' ? 'AI' : 'All'}
            </button>
          ))}
        </div>
      )}
      <div className="rp-search">
        <SearchBox value={q} onChange={setQ} placeholder="Pilot or aircraft…" />
        <button className={`rp-chip ${onlyAirborne ? 'on' : ''}`} onClick={() => setOnlyAirborne(v => !v)} title="Only flights in the air now">In the air</button>
      </div>
      <div className="rp-list">
        {shown.length === 0 && <div className="rp-empty">No flights</div>}
        {shown.map((f, n) => (
          <Fragment key={f.i}>
            {(n === 0 || shown[n - 1].color !== f.color) && (
              <div className="rp-tree-side" style={{ color: sideHex(f.color) }}>
                {f.color === 'Blue' ? 'BLUE' : f.color === 'Red' ? 'RED' : 'OTHER'} <span>{shown.filter(x => x.color === f.color).length}</span>
              </div>
            )}
            <FlightRowView f={f} live={t >= f.t0 && t <= f.t1} on={f.i === focus} onPick={onPick} />
          </Fragment>
        ))}
      </div>
    </>
  )
}

function SearchBox({ value, onChange, placeholder }: { value: string; onChange: (s: string) => void; placeholder: string }) {
  return (
    <label className="rp-searchbox">
      <Search size={12} />
      <input placeholder={placeholder} value={value} onChange={ev => onChange(ev.target.value)} />
    </label>
  )
}

function FlightRowView({ f, live, on, onPick }: { f: RecFlight; live: boolean; on: boolean; onPick: (i: number) => void }) {
  return (
    <button className={`rp-row ${on ? 'on' : ''} ${live ? 'live' : 'dim'}`} onClick={() => onPick(f.i)} title={live ? 'In the air now: click to follow' : 'Not in the air now: click to jump to it'}>
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

// ── the object tree ────────────────────────────────────────────────────

const CATS: { key: string; label: string; test: (k: Kind) => boolean; open: boolean }[] = [
  { key: 'air', label: 'Aircraft', test: k => k === 'air', open: true },
  { key: 'helo', label: 'Helicopters', test: k => k === 'helo', open: true },
  { key: 'wpn', label: 'Weapons', test: isWeapon, open: false },
  { key: 'ad', label: 'Air defence', test: k => k === 'sam', open: false },
  { key: 'gnd', label: 'Ground', test: k => k === 'armor' || k === 'vehicle' || k === 'infantry' || k === 'parachutist', open: false },
  { key: 'sea', label: 'Sea', test: k => k === 'ship' || k === 'carrier', open: false },
  { key: 'stat', label: 'Static & other', test: k => k === 'static' || k === 'misc' || k === 'bullseye', open: false },
]
const SIDES: [string | null, string][] = [['Blue', 'BLUE'], ['Red', 'RED'], [null, 'OTHER']]
const ROW_CAP = 250

/** Everything in the picture at `t`, by coalition and category, as
 *  Tacview's object list shows it. Effects (shells, flares) are left out. */
function ObjectTree({ meta, t, focus, measure, onPick }: {
  meta: RecMeta; t: number; focus: number | null; measure: number | null; onPick: (i: number, shift: boolean) => void
}) {
  const [q, setQ] = useState('')
  const [all, setAll] = useState(false)
  const [open, setOpen] = useState<Record<string, boolean>>({})
  // Refresh the membership once a second of replay, not every frame.
  const tick = Math.floor(t / 1000)
  const groups = useMemo(() => {
    const s = q.trim().toLowerCase()
    const out = new Map<string, number[]>()
    meta.objects.forEach((o, i) => {
      if (isEffect(o.k)) return
      if (!all && (t < o.t0 || t > o.t1)) return
      if (s && !`${o.p ?? ''} ${o.n ?? ''} ${o.g ?? ''} ${o.cs ?? ''}`.toLowerCase().includes(s)) return
      const side = o.c === 'Blue' || o.c === 'Red' ? o.c : null
      const cat = CATS.find(c => c.test(o.k))?.key ?? 'stat'
      const key = `${side}|${cat}`
      let g = out.get(key)
      if (!g) out.set(key, (g = []))
      g.push(i)
    })
    for (const g of out.values()) g.sort((a, b) => objLabel(meta.objects[a]).localeCompare(objLabel(meta.objects[b])))
    return out
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [meta, q, all, tick])
  const sections: ReactNode[] = []
  for (const [side, sideLabel] of SIDES) {
    const cats = CATS.map(c => ({ c, list: groups.get(`${side}|${c.key}`) ?? [] })).filter(x => x.list.length)
    if (!cats.length) continue
    const total = cats.reduce((n, x) => n + x.list.length, 0)
    sections.push(
      <div key={sideLabel} className="rp-tree-side" style={{ color: sideHex(side) }}>{sideLabel} <span>{total}</span></div>,
    )
    for (const { c, list } of cats) {
      const key = `${side}|${c.key}`
      const isOpen = open[key] ?? (c.open || !!q)
      sections.push(
        <button key={key} className="rp-tree-cat" onClick={() => setOpen(o => ({ ...o, [key]: !isOpen }))}>
          {isOpen ? '▾' : '▸'} {c.label} <span>{list.length}</span>
        </button>,
      )
      if (!isOpen) continue
      for (const i of list.slice(0, ROW_CAP)) {
        const o = meta.objects[i]
        const live = t >= o.t0 && t <= o.t1
        sections.push(
          <button
            key={i}
            className={`rp-tree-row ${i === focus ? 'on' : ''} ${i === measure ? 'tgt' : ''} ${live ? '' : 'dim'}`}
            onClick={ev => onPick(i, ev.shiftKey)}
            title="Click to follow; Shift-click to measure to it"
          >
            <span className="rp-row-name">{o.p ?? (o.n ?? o.k).replace(/_/g, ' ')}</span>
            {o.p && <span className="rp-row-sub">{(o.n ?? '').replace(/_/g, ' ')}</span>}
          </button>,
        )
      }
      if (list.length > ROW_CAP) sections.push(<div key={`${key}-more`} className="rp-empty">+{list.length - ROW_CAP} more: search to narrow</div>)
    }
  }
  return (
    <>
      <div className="rp-search">
        <SearchBox value={q} onChange={setQ} placeholder="Name, type, group…" />
        <button className={`rp-chip ${all ? 'on' : ''}`} onClick={() => setAll(v => !v)} title="Every object in the recording, not just those in the picture now">Whole recording</button>
      </div>
      <div className="rp-list">
        {sections.length ? sections : <div className="rp-empty">Nothing in the picture</div>}
      </div>
    </>
  )
}

const RANGE_KIND: Record<string, string> = {
  bomb: 'BOMB', strafe: 'GUNS', trap: 'TRAP', aar: 'AAR', missile: 'MSL', engagement: 'ACM', anti_ship: 'SHIP', sling: 'SLNG',
  landing: 'LAND', troops: 'TRPS', gunnery: 'GNRY', cas: 'CAS', sead: 'SEAD', hot_zone: 'HOT', low_level: 'LOLO', field_landing: 'PATT', csar: 'CSAR',
}

/**
 * The training range's graded results in this recording: jump to the moment
 * (following that pilot's flight) or open the full debrief on the range site,
 * where the same pass has its LSO sheet, score sheet and 3D replay.
 */
function RangeList({ meta, hits, site, t, onSeek }: {
  meta: RecMeta
  hits: RangeHit[]
  site: string
  t: number
  onSeek: (hit: RangeHit, flight: number | null) => void
}) {
  const flightOf = (h: RangeHit) => {
    const at = meta.start_ms + h.t
    const f = meta.flights.find(f => f.pilot.trim().toLowerCase() === h.pilot.trim().toLowerCase() && f.start_ms - 60_000 <= at && at <= f.end_ms + 60_000)
    return f?.i ?? null
  }
  return (
    <div className="rp-list">
      {hits.map(h => (
        <div key={h.id} className={`rp-ev rp-ev-range ${h.t <= t ? '' : 'dim'}`}>
          <button className="rp-ev-main" onClick={() => onSeek(h, flightOf(h))} title="Jump to this moment and follow the pilot">
            <span className="rp-ev-t">{fmtClock(meta.start_ms + h.t)}</span>
            <span className="rp-range-kind">{RANGE_KIND[h.kind] ?? h.kind.toUpperCase()}</span>
            <span>{h.pilot}{h.headline ? ` · ${h.headline}` : ''}</span>
          </button>
          {site && (
            <a className="rp-range-link" href={`${site}/result/${encodeURIComponent(h.id)}`} target="_blank" rel="noreferrer" title="The debrief on the range site">
              Debrief ↗
            </a>
          )}
        </div>
      ))}
    </div>
  )
}

function EventList({ meta, t, focus, onSeek }: { meta: RecMeta; t: number; focus: number | null; onSeek: (t: number) => void }) {
  const [mine, setMine] = useState(focus != null)
  const [shots, setShots] = useState(false)
  const list = useMemo(() => meta.events.filter(ev => {
    if (ev.k === 'fired' && !shots) return false
    if (mine && focus != null) return ev.o === focus || ev.by === focus
    return true
  }), [meta, mine, shots, focus])
  return (
    <>
      <div className="rp-search">
        {focus != null && <button className={`rp-chip ${mine ? 'on' : ''}`} onClick={() => setMine(v => !v)} title="Only events of the followed object">This object only</button>}
        <button className={`rp-chip ${shots ? 'on' : ''}`} onClick={() => setShots(v => !v)} title="Every weapon release too">Include shots</button>
      </div>
      <div className="rp-list">
        {list.length === 0 && <div className="rp-empty">No events</div>}
        {list.slice(0, 600).map((ev, i) => (
          <button key={i} className={`rp-ev rp-ev-${ev.k} ${ev.t <= t ? '' : 'dim'}`} onClick={() => onSeek(ev.t - 8_000)}>
            <span className="rp-ev-t">{fmtClock(meta.start_ms + ev.t)}</span>
            <span>{eventText(meta, ev)}</span>
          </button>
        ))}
      </div>
    </>
  )
}
