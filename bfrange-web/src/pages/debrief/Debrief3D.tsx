/**
 * The 3D debrief: the few minutes of the server's Tacview recording around
 * this result (bfdb `/api/range/result/<id>/clip`), replayed in a scene of
 * our own models. Cameras: chase, cockpit, LSO (carrier passes), target
 * (ground attack), free orbit. The HUD reads the numbers off the replay:
 * height, speed, heading, and for a carrier pass the LSO's distance, height,
 * glideslope and lineup with a lens, for a drop the time to impact.
 *
 * Keys: space play/pause, ← / → five seconds, 1–6 cameras.
 */
import { useCallback, useEffect, useRef, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Maximize2, Pause, Play, RotateCcw } from '@icons'
import { api } from '../../api'
import { Empty, Loading } from '../../components/States'
import { DebriefScene, type CamMode, type Readout } from '../../components/three/scene'
import { FT, NM } from '../../lib/geo'
import { fmt, fmtSigned, pad3 } from '../../lib/format'
import { usePref } from '../../lib/usePref'
import type { Clip, RangeRecord } from '../../types'

const SPEEDS = [0.25, 0.5, 1, 2, 4] as const
const CAMS: { m: CamMode; label: string; key: string }[] = [
  { m: 'chase', label: 'Chase', key: '1' },
  { m: 'cockpit', label: 'Cockpit', key: '2' },
  { m: 'lso', label: 'LSO', key: '3' },
  { m: 'deck', label: 'Deck', key: '4' },
  { m: 'target', label: 'Target', key: '5' },
  { m: 'free', label: 'Free', key: '6' },
]

interface Opts { trails: boolean; labels: boolean; glide: boolean }
const isOpts = (v: unknown): v is Opts => !!v && ['trails', 'labels', 'glide'].every(k => typeof (v as Record<string, unknown>)[k] === 'boolean')

const clock = (t: number) => {
  const s = Math.round(t)
  const a = Math.abs(s)
  return `${s < 0 ? '−' : '+'}${Math.floor(a / 60)}:${String(a % 60).padStart(2, '0')}`
}

/** The lens as the pilot sees it: the ball against the green datum bars. */
function Lens({ gse }: { gse: number }) {
  // the ball runs about ±0.8° across the lens's cells
  const off = Math.max(-1, Math.min(1, gse / 0.8))
  const lo = gse < -0.8
  return (
    <div className="d3-lens" title={`${fmtSigned(gse, 2)}° from the glideslope`}>
      <span className="datum l" />
      <span className="datum r" />
      <span className="cells" />
      <span className="ball" style={{ transform: `translateY(${-off * 34}px)`, background: lo ? '#ff3b30' : undefined }} />
    </div>
  )
}

function Hud({ r, kind }: { r: Readout; kind: string }) {
  const s = r.self
  if (!s) return null
  const p = r.pass
  const inGroove = kind === 'trap' && p && p.x_m > -20 && p.x_m < 1.6 * NM
  return (
    <div className="d3-hud" aria-live="off">
      <div className="d3-hud-col">
        <span><i>ALT</i>{fmt(s.z / FT)}<small>ft</small></span>
        <span><i>GS</i>{fmt(s.gs_kts)}<small>kt</small></span>
        <span><i>HDG</i>{pad3(s.yaw)}°</span>
        <span><i>VS</i>{fmtSigned(s.vs_fpm, 0)}<small>fpm</small></span>
        {r.tti !== null && <span style={{ color: 'var(--ball)' }}><i>TTI</i>{fmt(r.tti, 1)}<small>s</small></span>}
      </div>
      {inGroove && p && (
        <div className="d3-hud-col right">
          <Lens gse={p.gse_deg} />
          <span><i>DIST</i>{fmt(p.x_m / NM, 2)}<small>NM</small></span>
          <span><i>HGT</i>{fmt(p.hgt_ft)}<small>ft</small></span>
          <span><i>GSE</i>{fmtSigned(p.gse_deg, 2)}°</span>
          <span><i>LUE</i>{fmtSigned(p.lue_deg, 2)}°</span>
        </div>
      )}
    </div>
  )
}

function Player({ clip, rec }: { clip: Clip; rec: RangeRecord }) {
  const host = useRef<HTMLDivElement>(null)
  const sceneRef = useRef<DebriefScene | null>(null)
  const kind = rec.result.kind
  const night = 'night' in rec.result ? !!rec.result.night : false
  const [opts, setOpts] = usePref<Opts>('range.d3.opts', { trails: true, labels: true, glide: true }, isOpts)
  const [cam, setCam] = useState<CamMode>(kind === 'trap' ? 'lso' : 'chase')
  const [playing, setPlaying] = useState(false)
  const [speed, setSpeed] = useState<number>(1)
  // a carrier pass starts at the start of the clip; a drop or a fight a little before the event
  const startT = Math.max(clip.t0, kind === 'trap' ? clip.t0 : Math.min(-40, clip.t1 - 30))
  const [t, setT] = useState(startT)
  const [read, setRead] = useState<Readout | null>(null)
  const tRef = useRef(startT)
  const play = useRef({ playing: false, speed: 1 })
  useEffect(() => { play.current = { playing, speed } }, [playing, speed])
  const [hasBoat, setHasBoat] = useState(false)
  const [hasTarget, setHasTarget] = useState(false)

  useEffect(() => {
    if (!host.current) return
    const s = new DebriefScene(host.current, clip, { ...opts, night })
    sceneRef.current = s
    setHasBoat(s.hasBoat())
    setHasTarget(s.hasTarget())
    let raf = 0, last = performance.now(), lastUi = 0
    const loop = (now: number) => {
      const dt = Math.min(0.1, (now - last) / 1000)
      last = now
      if (play.current.playing) {
        tRef.current = Math.min(clip.t1, tRef.current + dt * play.current.speed)
        if (tRef.current >= clip.t1) setPlaying(false)
      }
      const r = s.update(tRef.current, dt)
      if (now - lastUi > 100) {
        lastUi = now
        setRead(r)
        setT(tRef.current)
      }
      raf = requestAnimationFrame(loop)
    }
    raf = requestAnimationFrame(loop)
    return () => { cancelAnimationFrame(raf); s.dispose(); sceneRef.current = null }
    // the scene is rebuilt only for a new clip; options are pushed below
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [clip])

  useEffect(() => { sceneRef.current?.setOpts({ ...opts, night }) }, [opts, night])
  // a camera the clip cannot have falls back to the chase view
  const usable = (m: CamMode) => (m === 'lso' || m === 'deck' ? hasBoat : m === 'target' ? hasTarget : true)
  const camNow: CamMode = usable(cam) ? cam : 'chase'
  useEffect(() => { sceneRef.current?.setMode(camNow) }, [camNow])

  const seek = useCallback((v: number) => {
    tRef.current = Math.max(clip.t0, Math.min(clip.t1, v))
    setT(tRef.current)
  }, [clip])

  const onKey = (e: React.KeyboardEvent) => {
    if (e.key === ' ') { e.preventDefault(); if (tRef.current >= clip.t1) seek(clip.t0); setPlaying(p => !p) }
    else if (e.key === 'ArrowLeft') { e.preventDefault(); seek(tRef.current - 5) }
    else if (e.key === 'ArrowRight') { e.preventDefault(); seek(tRef.current + 5) }
    else {
      const c = CAMS.find(x => x.key === e.key)
      if (c && usable(c.m)) setCam(c.m)
    }
  }

  const fullscreen = () => {
    const el = host.current?.parentElement
    if (!el) return
    if (document.fullscreenElement) document.exitFullscreen().catch(() => {})
    else el.requestFullscreen?.().catch(() => {})
  }

  return (
    <div className="d3" tabIndex={0} onKeyDown={onKey} aria-label="3D debrief. Space plays, arrow keys step, 1 to 6 change camera.">
      <div className="d3-stage">
        <div ref={host} className="d3-host" />
        {read && <Hud r={read} kind={kind} />}
        {!clip.complete && (
          <div className="d3-note">The server is still recording this session: the clip ends where the recording does for now.</div>
        )}
        <button className="d3-fs btn-range ghost sm" onClick={fullscreen} title="Full screen" aria-label="Full screen"><Maximize2 size={14} /></button>
      </div>
      <div className="d3-bar">
        <button className="btn-range sm" onClick={() => { if (tRef.current >= clip.t1) seek(clip.t0); setPlaying(p => !p) }} aria-label={playing ? 'Pause' : 'Play'}>
          {playing ? <Pause size={14} /> : <Play size={14} />}
        </button>
        <button className="btn-range ghost sm" onClick={() => { seek(clip.t0); setPlaying(false) }} title="Back to the start" aria-label="Back to the start"><RotateCcw size={14} /></button>
        <span className="mono text-[12px] w-[52px] text-right">{clock(t)}</span>
        <input
          type="range" className="scrub flex-1 min-w-[120px]" min={clip.t0} max={clip.t1} step={0.1} value={t}
          onChange={e => seek(Number(e.target.value))} aria-label="Time"
        />
        <div className="seg" role="group" aria-label="Speed">
          {SPEEDS.map(s => <button key={s} aria-pressed={speed === s} onClick={() => setSpeed(s)}>{s < 1 ? `${s === 0.25 ? '¼' : '½'}×` : `${s}×`}</button>)}
        </div>
      </div>
      <div className="d3-bar">
        <div className="seg" role="group" aria-label="Camera">
          {CAMS.filter(c => usable(c.m)).map(c => (
            <button key={c.m} aria-pressed={camNow === c.m} onClick={() => setCam(c.m)} title={`${c.label} (${c.key})`}>{c.label}</button>
          ))}
        </div>
        <label className="d3-check"><input type="checkbox" checked={opts.trails} onChange={() => setOpts(o => ({ ...o, trails: !o.trails }))} /> Trails</label>
        <label className="d3-check"><input type="checkbox" checked={opts.labels} onChange={() => setOpts(o => ({ ...o, labels: !o.labels }))} /> Labels</label>
        {hasBoat && <label className="d3-check"><input type="checkbox" checked={opts.glide} onChange={() => setOpts(o => ({ ...o, glide: !o.glide }))} /> Glideslope</label>}
        <span className="ml-auto dim text-[11.5px]">{clip.objects.length} objects from {clip.source || 'the recording'} · models to scale, small ones drawn larger far away</span>
      </div>
    </div>
  )
}

export function Debrief3D({ rec }: { rec: RangeRecord }) {
  const q = useQuery({
    queryKey: ['clip', rec.id],
    queryFn: () => api.clip(rec.id),
    staleTime: 60_000,
    retry: false,
  })
  if (q.isLoading) return <Loading label="Cutting the recording" />
  if (q.error || !q.data) {
    const msg = q.error instanceof Error ? q.error.message : 'no clip'
    return (
      <Empty title="No 3D replay for this result">
        {msg}. The 3D debrief is cut from the server's Tacview recording, so it needs Tacview recording on the range server
        (and a few seconds after the result for the recording to catch up).
        <button className="btn-range sm mt-3" onClick={() => q.refetch()}>Try again</button>
      </Empty>
    )
  }
  return <Player clip={q.data} rec={rec} />
}

export default Debrief3D
