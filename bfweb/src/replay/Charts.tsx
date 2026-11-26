// Tacview's telemetry charts: any recorded property of the followed object
// over the timeline's span -- altitude, speeds, Mach, G, AOA, fuel, throttle,
// whatever else the recording carries -- several at once, each on its own
// scale, with values read off at the cursor (or the playhead). Click to seek.
// Drawn on a canvas from the whole track.

import { useEffect, useMemo, useRef, useState } from 'react'
import { type Seg, type TrackSource, sampleSeg, isStepChannel } from './data'
import { gAt, iasOf, machOf, tasOf } from './analysis'
import { type Units, fmtAlt, fmtSpeed, fmtVs, fmtWeight } from './units'

interface Series {
  key: string
  label: string
  color: string
  /** value at sample i (state = sampleSeg at that sample's time) */
  value: (s: Seg, i: number) => number | null
  fmt: (v: number, u: Units) => string
}

const PALETTE = ['#b6f04a', '#4fd1e8', '#ffb547', '#ff6b8a', '#b48cff', '#7dff9b', '#ffe26b', '#ff8f4f']

function stateAt(s: Seg, i: number) { return sampleSeg(s, s.t[i]) }

const BUILTIN: Omit<Series, 'color'>[] = [
  { key: 'alt', label: 'ALT', value: (s, i) => s.alt[i], fmt: fmtAlt },
  { key: 'agl', label: 'AGL', value: (s, i) => (s.agl ? s.agl[i] : null), fmt: fmtAlt },
  { key: 'ias', label: 'IAS', value: (s, i) => { const st = stateAt(s, i); return st ? iasOf(st) : null }, fmt: fmtSpeed },
  { key: 'tas', label: 'TAS', value: (s, i) => s.ch?.TAS?.[i] ?? (() => { const st = stateAt(s, i); return st ? tasOf(st) : null })(), fmt: fmtSpeed },
  { key: 'gs', label: 'GS', value: (s, i) => stateAt(s, i)?.gs ?? null, fmt: fmtSpeed },
  { key: 'mach', label: 'MACH', value: (s, i) => (s.mach ? s.mach[i] : (() => { const st = stateAt(s, i); return st ? machOf(st) : null })()), fmt: v => v.toFixed(2) },
  { key: 'vs', label: 'VS', value: (s, i) => stateAt(s, i)?.vs ?? null, fmt: fmtVs },
  { key: 'g', label: 'G', value: (s, i) => s.ch?.VerticalGForce?.[i] ?? gAt(s, i), fmt: v => v.toFixed(1) },
  { key: 'aoa', label: 'AOA', value: (s, i) => (s.aoa ? s.aoa[i] : null), fmt: v => `${v.toFixed(1)}°` },
  { key: 'pitch', label: 'PITCH', value: (s, i) => (s.pitch ? s.pitch[i] : null), fmt: v => `${v.toFixed(0)}°` },
  { key: 'roll', label: 'BANK', value: (s, i) => (s.roll ? s.roll[i] : null), fmt: v => `${v.toFixed(0)}°` },
  { key: 'hdg', label: 'HDG', value: (s, i) => (s.yaw ? (s.yaw[i] + 360) % 360 : null), fmt: v => `${Math.round(v).toString().padStart(3, '0')}°` },
  { key: 'ch:FuelWeight', label: 'FUEL', value: (s, i) => s.ch?.FuelWeight?.[i] ?? null, fmt: fmtWeight },
  { key: 'ch:Throttle', label: 'THR', value: (s, i) => s.ch?.Throttle?.[i] ?? null, fmt: v => `${Math.round(v * 100)}%` },
]
const SHOWN = new Set(BUILTIN.map(b => b.key.replace(/^ch:/, '')).concat(['TAS', 'VerticalGForce']))

/** The series this track can draw: the built-ins it has data for, then its
 *  other channels by name. */
function seriesOf(s: Seg): Omit<Series, 'color'>[] {
  const out = BUILTIN.filter(b => {
    if (b.key === 'agl') return !!s.agl
    if (b.key === 'aoa') return !!s.aoa
    if (b.key === 'pitch' || b.key === 'roll' || b.key === 'hdg') return !!s.yaw
    if (b.key.startsWith('ch:')) return !!s.ch?.[b.key.slice(3)]
    return true
  })
  for (const k of Object.keys(s.ch ?? {}).sort()) {
    if (SHOWN.has(k) || /^LockedTarget\d?$/.test(k)) continue
    out.push({ key: `ch:${k}`, label: k, value: (sg, i) => sg.ch?.[k]?.[i] ?? null, fmt: v => (isStepChannel(k) ? String(Math.round(v)) : Math.abs(v) >= 100 ? Math.round(v).toLocaleString() : v.toFixed(2)) })
  }
  return out
}

const H = 96

export default function Charts({ store, idx, range, t, onSeek, version, units }: {
  store: TrackSource
  idx: number
  range: [number, number]
  t: number
  onSeek: (t: number) => void
  /** bumps when the track arrives */
  version: number
  units: Units
}) {
  const cv = useRef<HTMLCanvasElement>(null)
  const [picked, setPicked] = useState<string[]>(['alt', 'ias'])
  const [hover, setHover] = useState<number | null>(null)
  const [a, b] = range
  const span = Math.max(1, b - a)
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const seg = useMemo(() => store.wholeTrack(idx), [store, idx, version])
  const all = useMemo(() => (seg ? seriesOf(seg) : []), [seg])
  const active: Series[] = useMemo(
    () => picked.map(k => all.find(x => x.key === k)).filter((x): x is Omit<Series, 'color'> => !!x).map((x, i) => ({ ...x, color: PALETTE[i % PALETTE.length] })),
    [picked, all],
  )

  // Sampled columns: about two points per pixel across the span.
  const data = useMemo(() => {
    if (!seg || seg.t.length < 2) return null
    let lo = 0
    while (lo < seg.t.length - 1 && seg.t[lo + 1] < a) lo++
    let hi = seg.t.length - 1
    while (hi > 0 && seg.t[hi - 1] > b) hi--
    const stride = Math.max(1, Math.floor((hi - lo) / 2400))
    const idxs: number[] = []
    for (let i = lo; i <= hi; i += stride) idxs.push(i)
    return active.map(sr => {
      const ts: number[] = [], vs: number[] = []
      let min = Infinity, max = -Infinity
      for (const i of idxs) {
        const v = sr.value(seg, i)
        if (v == null || !Number.isFinite(v)) continue
        ts.push(seg.t[i]); vs.push(v)
        if (v < min) min = v
        if (v > max) max = v
      }
      if (min === max) { min -= 1; max += 1 }
      return { sr, ts, vs, min, max }
    })
  }, [seg, active, a, b])

  // Draw.
  useEffect(() => {
    const c = cv.current
    if (!c) return
    const dpr = Math.min(window.devicePixelRatio || 1, 2)
    const w = c.clientWidth
    if (c.width !== Math.round(w * dpr) || c.height !== Math.round(H * dpr)) {
      c.width = Math.round(w * dpr)
      c.height = Math.round(H * dpr)
    }
    const ctx = c.getContext('2d')
    if (!ctx) return
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    ctx.clearRect(0, 0, w, H)
    const x = (tt: number) => ((tt - a) / span) * w
    // quarter grid
    ctx.strokeStyle = 'rgba(255,255,255,0.06)'
    ctx.lineWidth = 1
    for (let k = 1; k < 4; k++) { ctx.beginPath(); ctx.moveTo(0, (H * k) / 4); ctx.lineTo(w, (H * k) / 4); ctx.stroke() }
    for (const d of data ?? []) {
      ctx.strokeStyle = d.sr.color
      ctx.lineWidth = 1.4
      ctx.beginPath()
      d.vs.forEach((v, i) => {
        const px = x(d.ts[i]), py = H - 4 - ((v - d.min) / (d.max - d.min)) * (H - 10)
        if (i) ctx.lineTo(px, py); else ctx.moveTo(px, py)
      })
      ctx.stroke()
    }
    const head = (tt: number, color: string) => { ctx.strokeStyle = color; ctx.beginPath(); ctx.moveTo(x(tt), 0); ctx.lineTo(x(tt), H); ctx.stroke() }
    head(t, '#ffffff')
    if (hover != null) { ctx.setLineDash([3, 3]); head(hover, 'rgba(255,255,255,0.5)'); ctx.setLineDash([]) }
  }, [data, t, hover, a, span])

  const at = hover ?? t
  const valueAt = (d: NonNullable<typeof data>[number]): string => {
    if (!d.ts.length) return '—'
    let lo = 0, hi = d.ts.length - 1
    while (lo < hi) { const m = (lo + hi) >> 1; if (d.ts[m] < at) lo = m + 1; else hi = m }
    return d.sr.fmt(d.vs[lo], units)
  }
  const tAt = (clientX: number) => {
    const r = cv.current?.getBoundingClientRect()
    return r ? a + ((clientX - r.left) / r.width) * span : null
  }
  const toggle = (k: string) => setPicked(p => (p.includes(k) ? p.filter(x => x !== k) : [...p, k].slice(-6)))

  return (
    <div className="rp-chart">
      <div className="rp-chart-keys">
        {all.map(sr => {
          const i = picked.indexOf(sr.key)
          const d = data?.find(x => x.sr.key === sr.key)
          return (
            <button key={sr.key} className={`rp-chart-key ${i >= 0 ? 'on' : ''}`} onClick={() => toggle(sr.key)}
              style={i >= 0 ? { color: PALETTE[i % PALETTE.length], borderColor: PALETTE[i % PALETTE.length] } : undefined}>
              {sr.label}{d && <b> {valueAt(d)}</b>}
            </button>
          )
        })}
        {!seg && <span className="rp-chart-wait">loading telemetry…</span>}
      </div>
      <canvas
        ref={cv} style={{ height: H }}
        onMouseMove={ev => setHover(tAt(ev.clientX))}
        onMouseLeave={() => setHover(null)}
        onClick={ev => { const x = tAt(ev.clientX); if (x != null) onSeek(x) }}
        aria-label="Telemetry chart"
      />
    </div>
  )
}
