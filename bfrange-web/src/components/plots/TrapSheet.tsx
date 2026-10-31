/**
 * Interactive trap sheet: a SIDE view (height above the deck vs distance,
 * with the glideslope and its ±0.4 / ±0.8 / ±1.5° bands and the ship's
 * hull under the deck line) over a TOP view (ground track against the
 * landing area: the real deck outline, painted landing area, wires, lens,
 * LSO platform, lineup wedges, quarter-mile arcs, the wake crossing). Hover
 * either view for a crosshair on both with distance, altitude, GSE, LUE, AoA
 * and closure.
 *
 * Frame (from the record): x = metres aft of the landing point (the
 * 3-wire) along the final bearing, y = metres right of the landing-area
 * centreline. The ship comes from lib/carrierDeck.ts, in the same frame the
 * engine grades in, so the track meets the deck where the jet did.
 *
 * Both views are drawn to scale in metres; the top view is equal-scale.
 * "Deck" zooms in on the ramp, the wires and the touchdown.
 */
import { useId, useMemo, useState } from 'react'
import { deckFor, deckInLanding, shipToLanding } from '../../lib/carrierDeck'
import { FT, NM } from '../../lib/geo'
import { fmt, fmtSigned } from '../../lib/format'
import type { GrooveSample, TrapResult } from '../../types'
import { linear, ticks, usePlotW, useSvgPointer } from './scale'
import { Iflols } from './Iflols'
import { ballSay } from '../../lib/lens'

const D2R = Math.PI / 180

type Zoom = 'deck' | 'groove' | 'pattern'

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

function Readout({ s, rampX }: { s: GrooveSample | null; rampX: number }) {
  if (!s) {
    return <div className="readout"><span>Hover or touch either view for a readout.</span></div>
  }
  const toRamp = s.x_m - rampX
  return (
    <div className="readout" aria-live="polite">
      <span>dist <b>{fmt(s.x_m / NM, 2)} NM</b> <span className="dim">({fmt(s.x_m / FT)} ft{toRamp > 0 ? `, ${fmt(toRamp / FT)} ft to the ramp` : ', over the deck'})</span></span>
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

const pts = (a: [number, number][], X: (x: number) => number, Y: (y: number) => number) => a.map(([x, y]) => `${X(x)},${Y(y)}`).join(' ')

export function TrapSheet({ samples, trap }: { samples: GrooveSample[]; trap: TrapResult }) {
  const [zoom, setZoom] = useState<Zoom>('groove')
  const clipId = useId().replace(/:/g, '')
  const PW = usePlotW()
  const narrow = PW < 600
  const geom = useMemo(() => deckFor(trap.carrier_type), [trap.carrier_type])
  const deck = useMemo(() => deckInLanding(geom), [geom])
  const GS = geom.glideslope_deg
  const deckFt = geom.deck_height_m / FT

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
  // where the wheels met the deck: the first sample over the deck at deck height
  const touchdown = useMemo(() => {
    for (let i = grooveStart; i < samples.length; i++) {
      const s = samples[i]
      if (s.x_m < deck.ramp_x && s.alt_ft <= 6) return s
    }
    return null
  }, [samples, grooveStart, deck.ramp_x])
  const caught = trap.outcome === 'trap' && trap.wire ? trap.wire : null

  // ── side view, metres along the final bearing ──
  const SW = PW, SH = narrow ? 260 : 300, sm = { l: 52, r: 16, t: 16, b: 34 }
  const sideMax = zoom === 'deck' ? 0.32 * NM : Math.min(1.25, Math.max(1.0, (groove[0]?.x_m ?? NM) / NM + 0.05)) * NM
  const sideMin = zoom === 'deck' ? deck.bow_x - 20 : -0.08 * NM
  const sx = linear([sideMax, sideMin], [sm.l, SW - sm.r])
  const inSide = groove.filter(s => s.x_m <= sideMax)
  const maxAlt = zoom === 'deck'
    ? Math.max(200, ...inSide.map(s => s.alt_ft + 30))
    : Math.max(600, ...inSide.map(s => s.alt_ft + 60))
  const sy = linear([-deckFt - 8, Math.min(maxAlt, 1100)], [SH - sm.b, sm.t])
  const slopeAlt = (xm: number, dDeg: number) => (Math.max(0, xm) * Math.tan((GS + dDeg) * D2R)) / FT
  const sideTicks = zoom === 'deck' ? [0.3, 0.2, 0.1, 0] : narrow ? [0, 0.5, 1] : [0, 0.25, 0.5, 0.75, 1, 1.25]

  // ── top view: equal scale, port up ──
  const TW = PW, tm = { l: 16, r: 16, t: 14, b: 26 }
  const topMax = zoom === 'deck' ? 0.32 * NM : Math.max(1.1 * NM, ...samples.map(s => s.x_m)) + 0.06 * NM
  const topMin = zoom === 'deck' ? deck.bow_x - 30 : -0.28 * NM
  const px = (TW - tm.l - tm.r) / (topMax - topMin)
  const deckYs = deck.outline.map(p => -p[1])
  const vLo = zoom === 'deck' ? Math.min(-70, ...deckYs) - 20 : -0.3 * NM
  const vHi = zoom === 'deck'
    ? Math.max(90, ...deckYs) + 20
    : zoom === 'groove' ? 0.42 * NM : Math.max(0.42 * NM, ...samples.map(s => -s.y_m)) + 0.08 * NM
  const TH = Math.round(tm.t + tm.b + (vHi - vLo) * px)
  const TX = linear([topMax, topMin], [tm.l, TW - tm.r])
  const tvUp = linear([vHi, vLo], [tm.t, TH - tm.b])
  const TY = (ym: number) => tvUp(-ym)

  // wake crossing: where the track crosses the ship's keel line extended aft
  const wake = useMemo(() => {
    for (let i = 1; i < samples.length; i++) {
      const a = samples[i - 1].y_m - deck.wakeY(samples[i - 1].x_m)
      const b = samples[i].y_m - deck.wakeY(samples[i].x_m)
      if (a < 0 && b >= 0 && samples[i].x_m > 200) return i
    }
    return null
  }, [samples, deck])
  const wakeAlt = trap.pattern.wake_alt_ft ?? (wake !== null ? samples[wake].alt_ft : null)

  const { ref: sideRef, pt: sidePt, handlers: sideHandlers } = useSvgPointer<SVGSVGElement>()
  const { ref: topRef, pt: topPt, handlers: topHandlers } = useSvgPointer<SVGSVGElement>()

  // nearest sample to the pointer (a few hundred samples: no need to memoise)
  let sideIdx: number | null = null
  if (sidePt) {
    const xm = sx.invert(sidePt.x)
    let bd = Infinity
    groove.forEach((s, i) => { const d = Math.abs(s.x_m - xm); if (d < bd) { bd = d; sideIdx = i + grooveStart } })
  }
  let topIdx: number | null = null
  if (topPt) {
    let bd = 48
    samples.forEach((s, i) => {
      const d = Math.hypot(TX(s.x_m) - topPt.x, TY(s.y_m) - topPt.y)
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

  // the ship's side, from the plan's elevation: x along the final bearing, ft above the deck
  const sideX = (f: number) => shipToLanding(geom, f, 0)[0]
  const sideZ = (z: number) => (z - geom.deck_height_m) / FT
  // the traced outline of the whole ship when the plan has one, else the hull and the island tiers
  const traced = geom.side.silhouette.length > 4
  const hull: [number, number][] = (traced ? geom.side.silhouette : geom.side.hull).map(([f, z]) => [sideX(f), sideZ(z)])
  const islandParts: [number, number][][] = traced ? [] : geom.side.island.map(v => v.map(([f, z]) => [sideX(f), sideZ(z)] as [number, number]))

  const lw = geom.landing_half_w_m
  const la = { x0: deck.ramp_x, x1: deck.landing_end_x }

  return (
    <div className="flex flex-col gap-3">
      <div className="flex flex-wrap items-center gap-3">
        <span className="caps">{geom.name} · landing point = {geom.wires_m.length ? `${geom.target_wire}-wire` : 'deck spot'} · ramp {fmt(deck.ramp_x / FT)} ft aft</span>
        <div className="seg ml-auto" role="group" aria-label="Zoom">
          <button aria-pressed={zoom === 'deck'} onClick={() => setZoom('deck')}>Deck</button>
          <button aria-pressed={zoom === 'groove'} onClick={() => setZoom('groove')}>Groove</button>
          <button aria-pressed={zoom === 'pattern'} onClick={() => setZoom('pattern')}>Whole pattern</button>
        </div>
      </div>
      <Readout s={cur} rampX={deck.ramp_x} />
      <figure className="m-0">
        <figcaption className="caps mb-1">Side view · {GS}° glideslope · bands ±0.4 / ±0.8 / ±1.5°</figcaption>
        <svg ref={sideRef} viewBox={`0 0 ${SW} ${SH}`} className="plot" {...sideHandlers} role="img" aria-label="Side view: height above the deck against distance">
          <defs>
            <clipPath id={`${clipId}s`}><rect x={sm.l} y={sm.t} width={SW - sm.l - sm.r} height={SH - sm.t - sm.b} /></clipPath>
          </defs>
          {POSITIONS.filter(p => p.from * NM <= sideMax).map((p, i) => (
            <g key={p.k}>
              <rect x={sx(p.from * NM)} y={sm.t} width={sx(p.to * NM) - sx(p.from * NM)} height={sy(0) - sm.t} fill={i % 2 ? 'transparent' : 'var(--plot-grid)'} opacity={0.4} />
              <text x={(sx(p.from * NM) + sx(p.to * NM)) / 2} y={sm.t + 12} textAnchor="middle" style={{ fill: 'var(--dim)', fontWeight: 600 }}>{p.k}</text>
            </g>
          ))}
          {ticks(0, sy.domain[1], zoom === 'deck' ? 5 : 6).map(a => (
            <g key={`a${a}`}>
              <line className="grid" x1={sm.l} x2={SW - sm.r} y1={sy(a)} y2={sy(a)} />
              <text x={sm.l - 6} y={sy(a) + 3} textAnchor="end">{a}</text>
            </g>
          ))}
          {sideTicks.map(v => (
            <g key={`x${v}`}>
              <line className="grid" x1={sx(v * NM)} x2={sx(v * NM)} y1={sm.t} y2={sy(0)} />
              <text x={sx(v * NM)} y={SH - sm.b + 14} textAnchor="middle">{v}</text>
            </g>
          ))}
          <text className="axis-label" x={SW - sm.r} y={SH - 4} textAnchor="end">NM to the landing point</text>
          <text className="axis-label" x={14} y={sm.t} transform={`rotate(-90 14 ${sm.t})`} textAnchor="end">ft above deck</text>
          <g clipPath={`url(#${clipId}s)`}>
            {/* the sea and the ship */}
            <rect x={sm.l} y={sy(-deckFt)} width={SW - sm.l - sm.r} height={SH} fill="var(--sky)" opacity={0.08} />
            <line x1={sm.l} x2={SW - sm.r} y1={sy(-deckFt)} y2={sy(-deckFt)} stroke="var(--sky)" strokeOpacity={0.35} />
            <polygon points={pts(hull, sx, sy)} fill="var(--panel-3)" stroke="var(--haze)" strokeWidth={1} />
            {islandParts.map((v, i) => v.length === 2
              ? <line key={i} x1={sx(v[0][0])} y1={sy(v[0][1])} x2={sx(v[1][0])} y2={sy(v[1][1])} stroke="var(--haze)" />
              : <polygon key={i} points={pts(v, sx, sy)} fill="var(--panel-3)" stroke="var(--haze)" strokeWidth={0.9} />)}
            <line x1={sx(deck.ramp_x)} x2={sx(deck.bow_x)} y1={sy(0)} y2={sy(0)} stroke="var(--chalk)" strokeWidth={2.5} />
            {deck.wires.map((x, i) => (
              <g key={i}>
                <line x1={sx(x)} x2={sx(x)} y1={sy(0) - 4} y2={sy(0) + 3} stroke={caught === i + 1 ? 'var(--ball)' : 'var(--chalk)'} strokeWidth={caught === i + 1 ? 2.5 : 1.2} />
                {zoom === 'deck' && <text x={sx(x)} y={sy(0) + 14} textAnchor="middle" style={{ fill: caught === i + 1 ? 'var(--ball)' : 'var(--dim)', fontSize: 9 }}>{i + 1}</text>}
              </g>
            ))}
            {zoom === 'deck' && <text x={sx(deck.ramp_x) + 4} y={sy(0) - 6} textAnchor="end" style={{ fontSize: 9.5 }}>ramp</text>}
            {bands.flatMap(b => (b.d === 0 ? [0] : [b.d, -b.d])).map(d => {
              const b = bands.find(x => x.d === Math.abs(d))!
              return (
                <line key={`gs${d}`} x1={sx(0)} y1={sy(0)} x2={sx(sideMax)} y2={sy(slopeAlt(sideMax, d))}
                  stroke={b.c} strokeDasharray={b.dash} strokeWidth={d === 0 ? 1.4 : 1.1} />
              )
            })}
            {groove.map((b, i) => {
              if (i === 0 || b.x_m > sideMax + 40) return null
              const a = groove[i - 1]
              const g = inGroove(b) && i + grooveStart >= rolledOut
              return <line key={i} x1={sx(a.x_m)} y1={sy(a.alt_ft)} x2={sx(b.x_m)} y2={sy(b.alt_ft)} stroke={g ? gseColor(b.gse_deg) : 'var(--sky)'} strokeWidth={g ? 2.6 : 1.6} strokeLinecap="round" opacity={g ? 1 : 0.8} />
            })}
            {touchdown && (
              <g>
                <circle cx={sx(touchdown.x_m)} cy={sy(0)} r={4} fill="none" stroke="var(--ball)" strokeWidth={1.5} />
                {zoom === 'deck' && <text x={sx(touchdown.x_m)} y={sy(0) - 10} textAnchor="middle" style={{ fill: 'var(--ball)', fontSize: 10 }}>touchdown</text>}
              </g>
            )}
            {cur && cur.x_m <= sideMax && cur.x_m >= sideMin && idx !== null && idx >= grooveStart && (
              <g pointerEvents="none">
                <line x1={sx(cur.x_m)} x2={sx(cur.x_m)} y1={sm.t} y2={SH - sm.b} stroke="var(--ball)" strokeWidth={1} />
                <circle cx={sx(cur.x_m)} cy={sy(cur.alt_ft)} r={5} fill="var(--ball)" stroke="var(--plot-bg)" strokeWidth={2} />
              </g>
            )}
          </g>
        </svg>
      </figure>
      <figure className="m-0">
        <figcaption className="flex items-center gap-3 flex-wrap mb-1">
          <span className="caps">Top view · landing-area centreline</span>
          {wakeAlt !== null && <span className="caps" style={{ color: 'var(--chalk)' }}>Wake alt {fmt(wakeAlt)} ft</span>}
          {touchdown && <span className="caps" style={{ color: 'var(--ball)' }}>Touchdown {fmt(Math.abs(touchdown.x_m) / FT)} ft {touchdown.x_m > 0 ? 'short of' : 'past'} the {geom.wires_m.length ? `${geom.target_wire}-wire` : 'landing point'} · {fmtSigned(touchdown.y_m / FT, 0)} ft lineup</span>}
        </figcaption>
        <svg ref={topRef} viewBox={`0 0 ${TW} ${TH}`} className="plot" {...topHandlers} role="img" aria-label="Top view: ground track against the landing area">
          <defs>
            <clipPath id={clipId}><rect x={tm.l} y={tm.t} width={TW - tm.l - tm.r} height={TH - tm.t - tm.b} /></clipPath>
          </defs>
          <g clipPath={`url(#${clipId})`}>
            <rect x={0} y={0} width={TW} height={TH} fill="var(--sky)" opacity={0.05} />
            {zoom !== 'deck' && [0.25, 0.5, 0.75, 1].map(r => {
              const rp = r * NM * px
              return (
                <g key={r}>
                  <path d={`M ${TX(0)} ${TY(0) - rp} A ${rp} ${rp} 0 0 0 ${TX(0)} ${TY(0) + rp}`} fill="none" className="grid-2" strokeDasharray="3 5" />
                  <text x={TX(r * NM) - 4} y={TY(0) - 5} textAnchor="end">{r} NM</text>
                </g>
              )
            })}
            {zoom === 'deck' && [100, 200, 300, 400, 500].map(m => (
              <g key={m}>
                <line x1={TX(m)} x2={TX(m)} y1={tm.t} y2={TH - tm.b} className="grid" />
                <text x={TX(m) - 3} y={TH - tm.b - 4} textAnchor="end">{fmt(m / FT)} ft</text>
              </g>
            ))}
            {[{ d: 3, c: 'var(--band-3)' }, { d: 1, c: 'var(--band-2)' }, { d: 0.5, c: 'var(--band-1)' }].map(w => {
              const L = 1.3 * NM
              const off = L * Math.tan(w.d * D2R)
              return (
                <path key={w.d} d={`M ${TX(deck.ramp_x)} ${TY(deck.ramp_x * Math.tan(w.d * D2R))} L ${TX(L)} ${TY(off)} M ${TX(deck.ramp_x)} ${TY(-deck.ramp_x * Math.tan(w.d * D2R))} L ${TX(L)} ${TY(-off)}`} stroke={w.c} strokeDasharray="2 4" fill="none" />
              )
            })}
            <line x1={TX(1.4 * NM)} y1={TY(0)} x2={TX(deck.ramp_x)} y2={TY(0)} stroke="var(--band-0)" strokeWidth={1} />
            <line x1={TX(1.4 * NM)} y1={TY(deck.wakeY(1.4 * NM))} x2={TX(deck.ramp_x)} y2={TY(deck.wakeY(deck.ramp_x))} stroke="var(--dim)" strokeDasharray="1 5" />

            {/* the ship: flight deck, painted landing area, wires, island, lens, LSO */}
            <polygon points={pts(deck.waterline, TX, TY)} fill="none" stroke="var(--haze)" strokeWidth={0.8} strokeDasharray="4 3" opacity={0.7} />
            <polygon points={pts(deck.outline, TX, TY)} fill="var(--panel-3)" stroke="var(--haze)" strokeWidth={1} strokeLinejoin="round" />
            {deck.elevators.map((e, i) => (
              <g key={`el${i}`}>
                <polygon points={pts(e, TX, TY)} fill="none" stroke="var(--haze)" strokeWidth={0.9} />
                {zoom === 'deck' && <text x={e.reduce((a, q) => a + TX(q[0]), 0) / e.length} y={e.reduce((a, q) => a + TY(q[1]), 0) / e.length + 3.5} textAnchor="middle" style={{ fontSize: 9, fill: 'var(--dim)' }}>{i + 1}</text>}
              </g>
            ))}
            <polygon points={pts([[la.x0, -lw], [la.x1, -lw], [la.x1, lw], [la.x0, lw]], TX, TY)} fill="var(--plot-bg)" opacity={0.55} />
            <line x1={TX(la.x0)} x2={TX(la.x1)} y1={TY(-lw)} y2={TY(-lw)} stroke="var(--chalk)" strokeOpacity={0.6} strokeDasharray="6 4" />
            <line x1={TX(la.x0)} x2={TX(la.x1)} y1={TY(lw)} y2={TY(lw)} stroke="var(--chalk)" strokeOpacity={0.6} strokeDasharray="6 4" />
            <line x1={TX(la.x0)} x2={TX(la.x1)} y1={TY(0)} y2={TY(0)} stroke="var(--chalk)" strokeOpacity={0.5} strokeDasharray="10 8" />
            {/* the foul line: clear of it to starboard is the parking area (angled decks) */}
            {geom.angle_deg !== 0 && <line x1={TX(la.x0 - 20)} x2={TX(la.x1)} y1={TY(lw + 5)} y2={TY(lw + 5)} stroke="var(--wave)" strokeOpacity={0.55} strokeDasharray="3 3" />}
            {deck.runs.map(([a, b], i) => (
              <line key={`run${i}`} x1={TX(a[0])} y1={TY(a[1])} x2={TX(b[0])} y2={TY(b[1])} stroke="var(--haze)" strokeDasharray="6 4" />
            ))}
            {deck.spots.map((q, i) => (
              <g key={`sp${i}`}>
                <circle cx={TX(q[0])} cy={TY(q[1])} r={zoom === 'deck' ? 6 : 2.5} fill="none" stroke="var(--chalk)" strokeOpacity={0.7} />
                {zoom === 'deck' && <text x={TX(q[0])} y={TY(q[1]) + 3} textAnchor="middle" style={{ fontSize: 8, fill: 'var(--chalk)' }}>{i + 1}</text>}
              </g>
            ))}
            {deck.wires.map((x, i) => (
              <g key={i}>
                <line x1={TX(x)} x2={TX(x)} y1={TY(-lw - 1)} y2={TY(lw + 1)} stroke={caught === i + 1 ? 'var(--ball)' : 'var(--chalk)'} strokeWidth={caught === i + 1 ? 2.4 : 1} opacity={caught === i + 1 ? 1 : 0.75} />
                {zoom === 'deck' && <text x={TX(x)} y={TY(-lw - 4)} textAnchor="middle" style={{ fill: caught === i + 1 ? 'var(--ball)' : 'var(--haze)', fontSize: 10 }}>{i + 1}</text>}
              </g>
            ))}
            {deck.cats.map(([a, b], i) => (
              <line key={i} x1={TX(a[0])} y1={TY(a[1])} x2={TX(b[0])} y2={TY(b[1])} stroke="var(--dim)" strokeWidth={1} />
            ))}
            <polygon points={pts(deck.island, TX, TY)} fill="var(--line-2)" stroke="var(--haze)" strokeWidth={1} />
            <polygon points={pts(deck.island_top, TX, TY)} fill="none" stroke="var(--haze)" strokeWidth={0.7} />
            {deck.lens && <circle cx={TX(deck.lens[0])} cy={TY(deck.lens[1])} r={zoom === 'deck' ? 3.5 : 2} fill="var(--ball)" />}
            {deck.lso && <rect x={TX(deck.lso[0]) - 3} y={TY(deck.lso[1]) - 3} width={6} height={6} fill="var(--datum)" />}
            {zoom === 'deck' && (
              <>
                {deck.lens && <text x={TX(deck.lens[0])} y={TY(deck.lens[1]) - 7} textAnchor="middle" style={{ fontSize: 9.5, fill: 'var(--ball)' }}>lens</text>}
                {deck.lso && <text x={TX(deck.lso[0])} y={TY(deck.lso[1]) - 7} textAnchor="middle" style={{ fontSize: 9.5, fill: 'var(--datum)' }}>LSO</text>}
              </>
            )}

            {samples.map((b, i) => {
              if (i === 0) return null
              const a = samples[i - 1]
              const g = i > rolledOut && inGroove(b)
              return <line key={i} x1={TX(a.x_m)} y1={TY(a.y_m)} x2={TX(b.x_m)} y2={TY(b.y_m)} stroke={g ? lueColor(b.lue_deg) : 'var(--sky)'} strokeWidth={g ? 2.4 : 1.6} strokeLinecap="round" opacity={g ? 1 : 0.8} />
            })}
            {touchdown && <circle cx={TX(touchdown.x_m)} cy={TY(touchdown.y_m)} r={4.5} fill="none" stroke="var(--ball)" strokeWidth={1.8} />}
            {wake !== null && zoom !== 'deck' && (
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
          <text x={TW - tm.r} y={TH - 8} textAnchor="end" className="axis-label">port ↑ · starboard ↓ · to scale</text>
        </svg>
      </figure>
      <div className="flex flex-wrap gap-x-4 gap-y-1 text-[11.5px] muted">
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--band-0)' }} /> on the ball / lined up</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--band-2)' }} /> (a little) 0.4–0.8° GS · 0.5–1° LU</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--tone-fair)' }} /> called · 0.8–1.5° · 1–3°</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--band-3)' }} /> _a lot_ · beyond</span>
        <span className="inline-flex items-center gap-1.5"><span className="dot" style={{ background: 'var(--sky)' }} /> pattern, and after the landing point</span>
        {caught && <span className="inline-flex items-center gap-1.5"><span style={{ width: 12, height: 2.5, background: 'var(--ball)' }} /> #{caught} wire</span>}
      </div>
      <LensRow groove={groove} cur={cur} waveoff={trap.outcome === 'waveoff'} />
    </div>
  )
}

/** The lens at X / IM / IC / AR, as the pilot saw it, and at the cursor. */
function LensRow({ groove, cur, waveoff }: { groove: GrooveSample[]; cur: GrooveSample | null; waveoff: boolean }) {
  const at = POSITIONS.map(p => {
    const v = groove.filter(s => s.x_m <= p.from * NM && s.x_m > p.to * NM).map(s => s.gse_deg)
    return { k: p.k, g: v.length ? v.reduce((a, b) => a + b, 0) / v.length : null }
  })
  const live = cur && cur.x_m > 0 && cur.x_m < 1.2 * NM ? cur : null
  const cellOf = (k: string, g: number | null, wo: boolean, note?: string) => (
    <div key={k} className="lens-cell">
      <div className="caps">{k}</div>
      <Iflols gse={g} waveoff={wo} title={`The lens at ${k}`} />
      <div className="mono text-[11.5px]">{g === null ? 'no reading' : `${fmtSigned(g, 2)}° · ${ballSay(g)}`}{note ? <span className="dim"> · {note}</span> : null}</div>
    </div>
  )
  return (
    <figure className="m-0 mt-3">
      <figcaption className="caps mb-1">The lens (IFLOLS) · the ball at each position · wave-off lights lit on a wave-off</figcaption>
      <div className="lens-row">
        {at.map((a, i) => cellOf(a.k, a.g, waveoff && i === at.length - 1))}
        {cellOf(live ? 'Cursor' : 'Cursor (hover)', live ? live.gse_deg : null, false, live ? `${fmt(live.x_m / NM, 2)} nm` : undefined)}
      </div>
    </figure>
  )
}
