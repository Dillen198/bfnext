/**
 * Interactive trap sheet: a SIDE view (height above the deck vs distance,
 * with the 3.5° glideslope and its ±0.4 / ±0.8 / ±1.5° bands) over a TOP
 * view (ground track against the angled-deck centreline, lineup wedges,
 * quarter-mile arcs, the wake crossing). Hover either view for a crosshair
 * on both with distance, altitude, GSE, LUE, AoA and closure.
 *
 * Frame (from the record): x = metres aft of the landing point along the
 * final bearing, y = metres right of the landing-area centreline.
 */
import { useId, useMemo, useState } from 'react'
import { FT, NM } from '../../lib/geo'
import { fmt, fmtSigned } from '../../lib/format'
import type { GrooveSample, TrapResult } from '../../types'
import { linear, ticks, useSvgPointer } from './scale'

const GS_DEG = 3.5
const DECK_ANGLE = 9.14
const D2R = Math.PI / 180
const A = DECK_ANGLE * D2R

/** Colour a glideslope error by the LSO band it falls in. */
function gseColor(g: number): string {
  const a = Math.abs(g)
  if (a <= 0.4) return 'var(--band-0)'
  if (a <= 0.8) return 'var(--band-2)'
  if (a <= 1.5) return 'var(--tone-fair)'
  return 'var(--band-3)'
}

function lueColor(l: number): string {
  const a = Math.abs(l)
  if (a <= 0.5) return 'var(--band-0)'
  if (a <= 1) return 'var(--band-2)'
  if (a <= 3) return 'var(--tone-fair)'
  return 'var(--band-3)'
}

/** LSO position windows along the groove, NM from the landing point. */
const POSITIONS = [
  { k: 'X', from: 0.75, to: 0.5 },
  { k: 'IM', from: 0.5, to: 0.25 },
  { k: 'IC', from: 0.25, to: 0.1 },
  { k: 'AR', from: 0.1, to: 0 },
]

/**
 * The ship in the landing-area frame. Its axis runs 9.14° right of the
 * landing area; (f, r) = metres forward of the stern / right of the keel.
 */
const STERN: [number, number] = [55, -9.5]
function shipToLanding(f: number, r: number): [number, number] {
  return [STERN[0] - f * Math.cos(A) + r * Math.sin(A), STERN[1] + f * Math.sin(A) + r * Math.cos(A)]
}
const HULL = ([
  [0, -18], [0, 22], [250, 38], [312, 22], [332, 2], [300, -12], [205, -42], [70, -46], [18, -30],
] as [number, number][]).map(([f, r]) => shipToLanding(f, r))
/** lateral position of the ship's wake (keel line extended aft) at distance x */
const wakeY = (x: number) => STERN[1] - (x - STERN[0]) * Math.tan(A)

function Readout({ s }: { s: GrooveSample | null }) {
  if (!s) {
    return <div className="readout"><span>Hover or touch either view for a readout.</span></div>
  }
  return (
    <div className="readout" aria-live="polite">
      <span>dist <b>{fmt(s.x_m / NM, 2)} NM</b> <span className="dim">({fmt(s.x_m / FT)} ft)</span></span>
      <span>alt <b>{fmt(s.alt_ft)} ft</b></span>
      <span>GSE <b style={{ color: gseColor(s.gse_deg) }}>{fmtSigned(s.gse_deg, 2)}°</b></span>
      <span>LUE <b style={{ color: lueColor(s.lue_deg) }}>{fmtSigned(s.lue_deg, 2)}°</b></span>
      <span>AoA <b>{s.aoa_deg === null ? '—' : `${fmt(s.aoa_deg, 1)}°`}</b></span>
      <span>closure <b>{fmt(s.closure_kts)} kt</b></span>
      <span>VS <b>{fmtSigned(s.vs_fpm, 0)} fpm</b></span>
      <span className="dim">t {fmt(s.t, 1)} s</span>
    </div>
  )
}

export function TrapSheet({ samples, trap }: { samples: GrooveSample[]; trap: TrapResult }) {
  const [zoom, setZoom] = useState<'groove' | 'pattern'>('groove')
  const clipId = useId().replace(/:/g, '')

  // the groove: from the aft-most point of the 180 onward, x only decreases
  const grooveStart = useMemo(() => {
    let best = 0
    samples.forEach((s, i) => { if (s.x_m > samples[best].x_m) best = i })
    return best
  }, [samples])
  const groove = useMemo(() => samples.slice(grooveStart), [samples, grooveStart])
  // rolled out on final: the first sample after the 180 within 8° of the
  // centreline -- before it the track is still turning, so colour it as pattern
  const rolledOut = useMemo(() => {
    for (let i = grooveStart; i < samples.length; i++) if (Math.abs(samples[i].lue_deg) <= 8) return i
    return grooveStart
  }, [samples, grooveStart])
  // past the landing point the pass is over (trap rollout, bolter, waveoff climb)
  const inGroove = (s: GrooveSample) => s.x_m > 25

  // ── side view ──
  const SW = 900, SH = 290, sm = { l: 52, r: 16, t: 16, b: 34 }
  const xMaxNm = Math.min(1.25, Math.max(1.0, (groove[0]?.x_m ?? NM) / NM + 0.05))
  const sx = linear([xMaxNm, -0.06], [sm.l, SW - sm.r])
  const maxAlt = Math.max(600, ...groove.filter(s => s.x_m / NM <= xMaxNm).map(s => s.alt_ft + 60))
  const sy = linear([0, Math.min(maxAlt, 1100)], [SH - sm.b, sm.t])
  const slopeAlt = (xNm: number, dDeg: number) => (Math.max(0, xNm) * NM * Math.tan((GS_DEG + dDeg) * D2R)) / FT

  // ── top view: equal scale, port up ──
  const TW = 900, tm = { l: 16, r: 16, t: 14, b: 26 }
  const xs = samples.map(s => s.x_m / NM)
  const vs = samples.map(s => -s.y_m / NM) // + = port (up the page)
  const maxX = Math.max(1.1, ...xs) + 0.06
  const minX = -0.28
  const px = (TW - tm.l - tm.r) / (maxX - minX)
  const vLo = -0.3
  const vHi = zoom === 'groove' ? 0.42 : Math.max(0.42, ...vs) + 0.08
  const TH = Math.round(tm.t + tm.b + (vHi - vLo) * px)
  const tx = linear([maxX, minX], [tm.l, TW - tm.r])
  const tv = linear([vHi, vLo], [tm.t, TH - tm.b])
  const TX = (xm: number) => tx(xm / NM)
  const TY = (ym: number) => tv(-ym / NM)

  // wake crossing: where the track crosses the ship's keel line extended aft
  const wake = useMemo(() => {
    for (let i = 1; i < samples.length; i++) {
      const a = samples[i - 1].y_m - wakeY(samples[i - 1].x_m)
      const b = samples[i].y_m - wakeY(samples[i].x_m)
      if (a < 0 && b >= 0 && samples[i].x_m > 200) return i
    }
    return null
  }, [samples])
  const wakeAlt = trap.pattern.wake_alt_ft ?? (wake !== null ? samples[wake].alt_ft : null)

  const { ref: sideRef, pt: sidePt, handlers: sideHandlers } = useSvgPointer<SVGSVGElement>()
  const { ref: topRef, pt: topPt, handlers: topHandlers } = useSvgPointer<SVGSVGElement>()

  // nearest sample to the pointer (a few hundred samples: no need to memoise)
  let sideIdx: number | null = null
  if (sidePt) {
    const xm = sx.invert(sidePt.x) * NM
    let bd = Infinity
    groove.forEach((s, i) => { const d = Math.abs(s.x_m - xm); if (d < bd) { bd = d; sideIdx = i + grooveStart } })
  }
  let topIdx: number | null = null
  if (topPt) {
    let bd = 48
    samples.forEach((s, i) => {
      const d = Math.hypot(tx(s.x_m / NM) - topPt.x, tv(-s.y_m / NM) - topPt.y)
      if (d < bd) { bd = d; topIdx = i }
    })
  }
  const idx = sideIdx ?? topIdx
  const cur = idx !== null ? samples[idx] : null

  const bands = [
    { d: 0, c: 'var(--band-0)', dash: '' },
    { d: 0.4, c: 'var(--band-1)', dash: '2 4' },
    { d: 0.8, c: 'var(--band-2)', dash: '2 4' },
    { d: 1.5, c: 'var(--band-3)', dash: '2 4' },
  ]

  return (
    <div className="flex flex-col gap-3">
      <Readout s={cur} />
      <figure className="m-0">
        <figcaption className="caps mb-1">Side view · {GS_DEG}° glideslope · bands ±0.4 / ±0.8 / ±1.5°</figcaption>
        <svg ref={sideRef} viewBox={`0 0 ${SW} ${SH}`} className="plot" {...sideHandlers} role="img" aria-label="Side view: height above the deck against distance">
          {POSITIONS.filter(p => p.from <= xMaxNm).map((p, i) => (
            <g key={p.k}>
              <rect x={sx(p.from)} y={sm.t} width={sx(p.to) - sx(p.from)} height={SH - sm.t - sm.b} fill={i % 2 ? 'transparent' : 'var(--plot-grid)'} opacity={0.4} />
              <text x={(sx(p.from) + sx(p.to)) / 2} y={sm.t + 12} textAnchor="middle" style={{ fill: 'var(--dim)', fontWeight: 600 }}>{p.k}</text>
            </g>
          ))}
          {ticks(0, sy.domain[1], 6).map(a => (
            <g key={`a${a}`}>
              <line className="grid" x1={sm.l} x2={SW - sm.r} y1={sy(a)} y2={sy(a)} />
              <text x={sm.l - 6} y={sy(a) + 3} textAnchor="end">{a}</text>
            </g>
          ))}
          {[0, 0.25, 0.5, 0.75, 1, 1.25].filter(v => v <= xMaxNm).map(v => (
            <g key={`x${v}`}>
              <line className="grid" x1={sx(v)} x2={sx(v)} y1={sm.t} y2={SH - sm.b} />
              <text x={sx(v)} y={SH - sm.b + 14} textAnchor="middle">{v}</text>
            </g>
          ))}
          <text className="axis-label" x={SW - sm.r} y={SH - 4} textAnchor="end">NM to the landing point</text>
          <text className="axis-label" x={14} y={sm.t} transform={`rotate(-90 14 ${sm.t})`} textAnchor="end">ft above deck</text>
          {bands.flatMap(b => (b.d === 0 ? [0] : [b.d, -b.d])).map(d => {
            const b = bands.find(x => x.d === Math.abs(d))!
            return (
              <line key={`gs${d}`} x1={sx(0)} y1={sy(0)} x2={sx(xMaxNm)} y2={sy(slopeAlt(xMaxNm, d))}
                stroke={b.c} strokeDasharray={b.dash} strokeWidth={d === 0 ? 1.4 : 1.1} />
            )
          })}
          {groove.map((b, i) => {
            if (i === 0 || b.x_m / NM > xMaxNm + 0.02) return null
            const a = groove[i - 1]
            const g = inGroove(b) && i + grooveStart >= rolledOut
            return <line key={i} x1={sx(a.x_m / NM)} y1={sy(a.alt_ft)} x2={sx(b.x_m / NM)} y2={sy(b.alt_ft)} stroke={g ? gseColor(b.gse_deg) : 'var(--sky)'} strokeWidth={g ? 2.6 : 1.6} strokeLinecap="round" opacity={g ? 1 : 0.8} />
          })}
          <line x1={sx(0.03)} x2={sx(-0.06)} y1={sy(0)} y2={sy(0)} stroke="var(--chalk)" strokeWidth={4} />
          {cur && cur.x_m / NM <= xMaxNm && cur.x_m / NM >= -0.06 && idx !== null && idx >= grooveStart && (
            <g pointerEvents="none">
              <line x1={sx(cur.x_m / NM)} x2={sx(cur.x_m / NM)} y1={sm.t} y2={SH - sm.b} stroke="var(--ball)" strokeWidth={1} />
              <circle cx={sx(cur.x_m / NM)} cy={sy(cur.alt_ft)} r={5} fill="var(--ball)" stroke="var(--plot-bg)" strokeWidth={2} />
            </g>
          )}
        </svg>
      </figure>
      <figure className="m-0">
        <figcaption className="flex items-center gap-3 flex-wrap mb-1">
          <span className="caps">Top view · angled-deck centreline</span>
          {wakeAlt !== null && <span className="caps" style={{ color: 'var(--chalk)' }}>Wake alt {fmt(wakeAlt)} ft</span>}
          <div className="seg ml-auto" role="group" aria-label="Top view zoom">
            <button aria-pressed={zoom === 'groove'} onClick={() => setZoom('groove')}>Groove</button>
            <button aria-pressed={zoom === 'pattern'} onClick={() => setZoom('pattern')}>Whole pattern</button>
          </div>
        </figcaption>
        <svg ref={topRef} viewBox={`0 0 ${TW} ${TH}`} className="plot" {...topHandlers} role="img" aria-label="Top view: ground track against the landing-area centreline">
          <defs>
            <clipPath id={clipId}><rect x={tm.l} y={tm.t} width={TW - tm.l - tm.r} height={TH - tm.t - tm.b} /></clipPath>
          </defs>
          <g clipPath={`url(#${clipId})`}>
            {[0.25, 0.5, 0.75, 1].map(r => {
              const rp = r * px
              return (
                <g key={r}>
                  <path d={`M ${tx(0)} ${tv(0) - rp} A ${rp} ${rp} 0 0 0 ${tx(0)} ${tv(0) + rp}`} fill="none" className="grid-2" strokeDasharray="3 5" />
                  <text x={tx(r) - 4} y={tv(0) - 5} textAnchor="end">{r} NM</text>
                </g>
              )
            })}
            {[{ d: 3, c: 'var(--band-3)' }, { d: 1, c: 'var(--band-2)' }, { d: 0.5, c: 'var(--band-1)' }].map(w => {
              const L = 1.3 * NM
              const off = L * Math.tan(w.d * D2R)
              return (
                <path key={w.d} d={`M ${TX(0)} ${TY(0)} L ${TX(L)} ${TY(-off)} M ${TX(0)} ${TY(0)} L ${TX(L)} ${TY(off)}`} stroke={w.c} strokeDasharray="2 4" fill="none" />
              )
            })}
            <line x1={TX(1.4 * NM)} y1={TY(0)} x2={TX(-420)} y2={TY(0)} stroke="var(--band-0)" strokeWidth={1} />
            <line x1={TX(1.4 * NM)} y1={TY(wakeY(1.4 * NM))} x2={TX(STERN[0])} y2={TY(STERN[1])} stroke="var(--dim)" strokeDasharray="1 5" />
            <polygon points={HULL.map(([x, y]) => `${TX(x)},${TY(y)}`).join(' ')} fill="var(--panel-3)" stroke="var(--haze)" strokeWidth={1} />
            {samples.map((b, i) => {
              if (i === 0) return null
              const a = samples[i - 1]
              const g = i > rolledOut && inGroove(b)
              return <line key={i} x1={TX(a.x_m)} y1={TY(a.y_m)} x2={TX(b.x_m)} y2={TY(b.y_m)} stroke={g ? lueColor(b.lue_deg) : 'var(--sky)'} strokeWidth={g ? 2.4 : 1.6} strokeLinecap="round" opacity={g ? 1 : 0.8} />
            })}
            {wake !== null && (
              <g>
                <circle cx={TX(samples[wake].x_m)} cy={TY(samples[wake].y_m)} r={5} fill="none" stroke="var(--chalk)" strokeWidth={1.5} />
                <text x={TX(samples[wake].x_m) + 9} y={TY(samples[wake].y_m) - 8} style={{ fill: 'var(--chalk)', fontSize: 11 }}>
                  Wake Alt: {fmt(wakeAlt)}ft
                </text>
              </g>
            )}
            {cur && (
              <g pointerEvents="none">
                <line x1={TX(cur.x_m)} x2={TX(cur.x_m)} y1={tm.t} y2={TH - tm.b} stroke="var(--ball)" strokeWidth={1} opacity={0.6} />
                <circle cx={TX(cur.x_m)} cy={TY(cur.y_m)} r={5} fill="var(--ball)" stroke="var(--plot-bg)" strokeWidth={2} />
              </g>
            )}
          </g>
          <text x={TW - tm.r} y={TH - 8} textAnchor="end" className="axis-label">port ↑ · starboard ↓ · equal scale</text>
        </svg>
      </figure>
      <div className="flex flex-wrap gap-x-4 gap-y-1 text-[11.5px] muted">
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--band-0)' }} /> on the ball / lined up</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--band-2)' }} /> (a little) 0.4–0.8° GS · 0.5–1° LU</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--tone-fair)' }} /> called · 0.8–1.5° · 1–3°</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--band-3)' }} /> _a lot_ · beyond</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--sky)' }} /> pattern, and after the landing point</span>
      </div>
    </div>
  )
}
