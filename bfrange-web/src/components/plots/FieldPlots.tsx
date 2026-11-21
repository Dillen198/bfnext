/**
 * A field landing, debriefed the way an instructor walks it: the pattern
 * from above (to scale, in the runway frame, every leg in its colour and
 * the points that matter marked: the break, abeam, the base, wings level on
 * final, the touchdown), the height through the pattern, the final against
 * the 3° glidepath, and after the wheels are down the rollout and the taxi
 * on the airfield with its speed.
 *
 * The legs come from lib/patternLegs.ts, the same reading the engine made
 * for the record's `pattern` summary.
 */
import { useMemo, useState, type SVGProps } from 'react'
import { fmt, fmtSigned } from '../../lib/format'
import { legs, relPoints, runwayFrame, TAXI_LIMIT_KTS, type Leg, type RelPt } from '../../lib/patternLegs'
import type { FieldLandingResult, TrackPt } from '../../types'
import { linear, ticks, useNarrow, usePlotW, useSvgPointer } from './scale'
import { TrackMap } from './TrackMap'

const NM = 1852
const FT = 3.28084

const LEG_COLOR: Record<Leg, string> = {
  initial: 'var(--sky)',
  break: '#c28cff',
  downwind: 'var(--ball)',
  base: 'var(--tone-fair)',
  final: 'var(--datum)',
  rollout: 'var(--chalk)',
  taxi: 'var(--haze)',
  other: 'var(--dim)',
}
const LEG_LABEL: Record<Leg, string> = {
  initial: 'Initial', break: 'Break', downwind: 'Downwind', base: 'Base', final: 'Final', rollout: 'Rollout', taxi: 'Taxi', other: 'Arrival',
}

const pts = (a: [number, number][]) => a.map(([x, y]) => `${x.toFixed(1)},${y.toFixed(1)}`).join(' ')

function Legend({ shown }: { shown: Leg[] }) {
  return (
    <div className="flex flex-wrap gap-x-4 gap-y-1 text-[11.5px] muted">
      {shown.map(l => (
        <span key={l} className="inline-flex items-center gap-1.5">
          <span style={{ width: 14, height: 3, background: LEG_COLOR[l], display: 'inline-block' }} /> {LEG_LABEL[l]}
        </span>
      ))}
    </div>
  )
}

function Marker({ x, y, label, color = 'var(--chalk)', below = false, left = false, shape = 'dot' }: { x: number; y: number; label: string; color?: string; below?: boolean; left?: boolean; shape?: 'dot' | 'x' | 'ring' }) {
  return (
    <g>
      {shape === 'x' ? (
        <g stroke={color} strokeWidth={2.2}>
          <line x1={x - 5} y1={y - 5} x2={x + 5} y2={y + 5} />
          <line x1={x + 5} y1={y - 5} x2={x - 5} y2={y + 5} />
        </g>
      ) : (
        <circle cx={x} cy={y} r={shape === 'ring' ? 5 : 4} fill={shape === 'ring' ? 'none' : color} stroke={shape === 'ring' ? color : 'var(--plot-bg)'} strokeWidth={1.6} />
      )}
      <text x={left ? x - 7 : x + 7} y={below ? y + 15 : y - 8} textAnchor={left ? 'end' : 'start'} style={{ fontSize: 10.5, fill: color, fontWeight: 600 }}>{label}</text>
    </g>
  )
}

/**
 * The pattern from above, to scale. Landing left to right (right of the
 * centreline down the page); on a phone the page turns so the landing is up
 * it, which fits a pattern's shape to a tall screen.
 */
export function PatternPlan({ r, track }: { r: FieldLandingResult; track: TrackPt[] }) {
  const rel = useMemo(() => relPoints(track, r), [track, r])
  const lg = useMemo(() => legs(rel, 0), [rel])
  const up = useNarrow()
  const p = r.pattern
  const L = r.runway_length_m || 2400
  const Wd = r.runway_width_m || 45
  const W = up ? 440 : 900, m = 18
  const xs = rel.map(q => q.along), ys = rel.map(q => q.across)
  const lo = Math.min(-0.5 * NM, ...xs) - 300, hi = Math.max(L + 300, ...xs) + 300
  const yl = Math.min(-0.4 * NM, ...ys) - 300, yh = Math.max(0.4 * NM, ...ys) + 300
  // page-across and page-down spans of the drawing, then one scale that fits both
  const [sx, sy] = up ? [yh - yl, hi - lo] : [hi - lo, yh - yl]
  const H = Math.round(Math.min(up ? 760 : 560, Math.max(260, sy * ((W - 2 * m) / sx) + 2 * m)))
  const k = Math.min((W - 2 * m) / sx, (H - 2 * m) / sy)
  const am = (lo + hi) / 2, cm = (yl + yh) / 2
  const P = (along: number, across: number): [number, number] =>
    up ? [W / 2 + (across - cm) * k, H / 2 - (along - am) * k] : [W / 2 + (along - am) * k, H / 2 + (across - cm) * k]
  const { ref, pt, handlers } = useSvgPointer<SVGSVGElement>()
  let cur: RelPt | null = null, curLeg: Leg | null = null
  if (pt) {
    let bd = 30
    rel.forEach((q, i) => { const [x, y] = P(q.along, q.across); const d = Math.hypot(x - pt.x, y - pt.y); if (d < bd) { bd = d; cur = q; curLeg = lg[i] } })
  }
  const shown = [...new Set(lg)].filter(l => l !== 'other') as Leg[]
  const near = (along: number | null | undefined) => (along == null ? null : rel.reduce<RelPt | null>((b, q) => (b === null || Math.abs(q.along - along) < Math.abs(b.along - along) ? q : b), null))
  const abeamPt = p?.abeam_nm != null ? rel.filter((_, i) => lg[i] === 'downwind').reduce<RelPt | null>((b, q) => (b === null || Math.abs(q.along) < Math.abs(b.along) ? q : b), null) : null
  const breakPt = p?.break_at_m != null ? rel.filter((_, i) => lg[i] === 'initial').at(-1) ?? near(p.break_at_m) : null
  const basePt = rel.find((_, i) => lg[i] === 'base') ?? null
  const rollPt = rel.find((_, i) => lg[i] === 'final') ?? null
  const c = cur as RelPt | null
  /** a mark at a track point, its label kept on the page */
  const patSide = p?.side === 'right' ? 1 : -1
  const mark = (q: { along: number; across: number }, label: string, color: string, shape?: 'x', flip = false) => {
    const [x, y] = P(q.along, q.across)
    // on the centreline: away from the pattern (`flip`: towards it); on the pattern: outside it
    const out = Math.abs(q.across) < 300 ? (flip ? patSide : -patSide) : Math.sign(q.across)
    const w = label.length * 6.4 + 10
    if (up) {
      const left = out < 0 ? x - w > 0 : x + w > W
      return <Marker x={x} y={y} label={label} color={color} shape={shape} left={left} below={y < 40} />
    }
    return <Marker x={x} y={y} label={label} color={color} shape={shape} below={out > 0} left={x + w > W} />
  }
  const line = (a: [number, number], b: [number, number], props: SVGProps<SVGLineElement>) => <line x1={a[0]} y1={a[1]} x2={b[0]} y2={b[1]} {...props} />
  const half = Math.max(Wd / 2, 1 / k)
  const rwy = [P(0, -half), P(L, -half), P(L, half), P(0, half)]
  const thr = P(0, 0)
  return (
    <figure className="m-0">
      <figcaption className="caps mb-1">
        Pattern · from above, to scale · {p ? `${p.kind.replace('_', '-')}${p.side ? `, ${p.side}-hand` : ''}` : 'no pattern summary'}
      </figcaption>
      <div className="readout">
        {c ? (
          <>
            <span><b style={{ color: curLeg ? LEG_COLOR[curLeg] : undefined }}>{curLeg ? LEG_LABEL[curLeg] : ''}</b></span>
            <span>{fmt(Math.abs(c.along) / NM, 2)} nm {c.along < 0 ? 'before' : 'past'} the threshold</span>
            <span>{fmt(Math.abs(c.across) / NM, 2)} nm {c.across < 0 ? 'left' : 'right'}</span>
            <span><b>{fmt(c.agl * FT)}</b> ft AGL</span>
            <span><b>{fmt(c.gs_kts)}</b> kt</span>
            <span className="dim">t {fmtSigned(c.t, 0)} s</span>
          </>
        ) : <span>{up ? 'Touch the track for a readout.' : 'Hover the track for a readout.'}</span>}
      </div>
      <svg ref={ref} viewBox={`0 0 ${W} ${H}`} className="plot" {...handlers} role="img" aria-label="The traffic pattern from above">
        {/* extended centreline with nm ticks, the runway to scale */}
        {line(P(lo, 0), P(hi, 0), { className: 'grid-2', strokeDasharray: '8 6' })}
        {[1, 2, 3, 4, 5].filter(n => -n * NM > lo).map(n => {
          const [x, y] = P(-n * NM, 0)
          return (
            <g key={n}>
              {line(P(-n * NM, -5 / k), P(-n * NM, 5 / k), { stroke: 'var(--haze)' })}
              <text x={up ? x + 8 : x} y={up ? y + 3 : y + 17} textAnchor={up ? 'start' : 'middle'}>{n} nm</text>
            </g>
          )
        })}
        {[0.5, 1, 1.5, 2].flatMap(n => [-n, n]).map(n => {
          const [x, y] = P(hi, n * NM)
          return (
            <g key={`a${n}`}>
              {line(P(lo, n * NM), P(hi, n * NM), { className: 'grid', strokeDasharray: '2 6' })}
              {(up ? x > 4 && x + 46 < W : y > 12 && y < H - 4) && <text x={up ? x + 3 : x - 4} y={up ? y + 12 : y - 3} textAnchor={up ? 'start' : 'end'}>{Math.abs(n)} nm {n < 0 ? 'L' : 'R'}</text>}
            </g>
          )
        })}
        <polygon points={pts(rwy)} fill="var(--line-2)" stroke="var(--haze)" />
        <text x={up ? thr[0] - patSide * 8 : thr[0] - 4} y={up ? thr[1] + 4 : thr[1] - 8} textAnchor={up ? (patSide < 0 ? 'start' : 'end') : 'end'} style={{ fill: 'var(--chalk)', fontWeight: 600 }}>RWY {r.runway}</text>
        {/* the track, leg by leg */}
        {rel.map((q, i) => {
          if (i === 0) return null
          const [a, b] = [P(rel[i - 1].along, rel[i - 1].across), P(q.along, q.across)]
          return <line key={i} x1={a[0]} y1={a[1]} x2={b[0]} y2={b[1]} stroke={LEG_COLOR[lg[i]]} strokeWidth={2.2} strokeLinecap="round" />
        })}
        {breakPt && p?.break_alt_ft != null && mark(breakPt, `BREAK ${fmt(p.break_alt_ft)} ft`, '#c28cff')}
        {abeamPt && p?.abeam_nm != null && mark(abeamPt, `ABEAM ${fmt(p.abeam_nm, 1)} nm · ${fmt(p.abeam_alt_ft ?? 0)} ft`, 'var(--ball)')}
        {basePt && mark(basePt, 'BASE', 'var(--tone-fair)')}
        {rollPt && p?.rollout_nm != null && mark(rollPt, `WINGS LEVEL ${fmt(p.rollout_nm, 2)} nm · ${fmt(p.rollout_alt_ft ?? 0)} ft`, 'var(--datum)')}
        {mark({ along: r.touchdown_from_threshold_m, across: r.centreline_m }, 'TD', 'var(--wave)', 'x', !up)}
        {c && (() => { const [x, y] = P(c.along, c.across); return <circle cx={x} cy={y} r={5} fill="var(--ball)" stroke="var(--plot-bg)" strokeWidth={2} pointerEvents="none" /> })()}
        <text x={W - m} y={H - 6} textAnchor="end" className="axis-label">landing direction {up ? '↑' : '→'} · to scale</text>
      </svg>
      <div className="mt-1"><Legend shown={shown} /></div>
    </figure>
  )
}

/** Height and speed through the pattern against time to touchdown. */
export function PatternProfile({ r, track }: { r: FieldLandingResult; track: TrackPt[] }) {
  const rel = useMemo(() => relPoints(track, r), [track, r])
  const lg = useMemo(() => legs(rel, 0), [rel])
  const pre = rel.filter(q => q.t <= 0)
  const [show, setShow] = useState<'alt' | 'gs'>('alt')
  const W = usePlotW()
  if (pre.length < 3) return null
  const H = 230, m = { l: 48, r: 14, t: 14, b: 30 }
  const t0 = pre[0].t
  const X = linear([t0, 0], [m.l, W - m.r])
  const v = (q: RelPt) => (show === 'alt' ? q.agl * FT : q.gs_kts)
  const vmax = Math.max(show === 'alt' ? 600 : 100, ...pre.map(v)) * 1.1
  const Y = linear([0, vmax], [H - m.b, m.t])
  const p = r.pattern
  const ref = (val: number | null | undefined, label: string, color: string) =>
    val == null ? null : (
      <g>
        <line x1={m.l} x2={W - m.r} y1={Y(val)} y2={Y(val)} stroke={color} strokeDasharray="3 5" opacity={0.6} />
        <text x={W - m.r - 4} y={Y(val) - 4} textAnchor="end" style={{ fill: color, fontSize: 10 }}>{label} {fmt(val)}</text>
      </g>
    )
  return (
    <figure className="m-0">
      <figcaption className="flex flex-wrap items-center gap-x-3 gap-y-1 mb-1">
        <span className="caps min-w-0">Through the pattern · {show === 'alt' ? 'height AGL' : 'ground speed'} against time to touchdown</span>
        <div className="seg ml-auto shrink-0" role="group" aria-label="Value">
          <button aria-pressed={show === 'alt'} onClick={() => setShow('alt')}>Height</button>
          <button aria-pressed={show === 'gs'} onClick={() => setShow('gs')}>Speed</button>
        </div>
      </figcaption>
      <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label="Height through the pattern">
        {ticks(0, vmax, 5).map(a => (
          <g key={a}>
            <line className="grid" x1={m.l} x2={W - m.r} y1={Y(a)} y2={Y(a)} />
            <text x={m.l - 6} y={Y(a) + 3} textAnchor="end">{a}</text>
          </g>
        ))}
        {ticks(t0, 0, W < 600 ? 5 : 8).map(t => (
          <g key={`t${t}`}>
            <line className="grid" x1={X(t)} x2={X(t)} y1={m.t} y2={H - m.b} />
            <text x={X(t)} y={H - m.b + 14} textAnchor="middle">{t === 0 ? 'TD' : `${t}s`}</text>
          </g>
        ))}
        {show === 'alt' && ref(p?.initial_alt_ft, 'initial', 'var(--sky)')}
        {show === 'alt' && ref(p?.downwind_alt_ft, 'downwind', 'var(--ball)')}
        {show === 'gs' && ref(p?.final_gs_kts, 'final', 'var(--datum)')}
        {pre.map((q, i) => i === 0 ? null : (
          <line key={i} x1={X(pre[i - 1].t)} y1={Y(v(pre[i - 1]))} x2={X(q.t)} y2={Y(v(q))} stroke={LEG_COLOR[lg[i]]} strokeWidth={2.2} strokeLinecap="round" />
        ))}
        <text x={14} y={m.t} transform={`rotate(-90 14 ${m.t})`} textAnchor="end" className="axis-label">{show === 'alt' ? 'ft AGL' : 'kt'}</text>
      </svg>
    </figure>
  )
}

/** The final against a 3° glidepath to the aim point, distance to the threshold across. */
export function FinalProfile({ r, track }: { r: FieldLandingResult; track: TrackPt[] }) {
  const rel = useMemo(() => relPoints(track, r), [track, r])
  const lg = useMemo(() => legs(rel, 0), [rel])
  const fin = rel.filter((q, i) => lg[i] === 'final' && q.t <= 0)
  const W = usePlotW()
  if (fin.length < 2) return null
  const aim = r.aim_point_m ?? 300
  const H = 220, m = { l: 48, r: 14, t: 14, b: 30 }
  const far = Math.min(3 * NM, Math.max(1.2 * NM, ...fin.map(q => -q.along + 200)))
  // approach from the left, the threshold, the runway to the aim point and past it on the right
  const X = linear([-far, Math.max(200, aim + 200)], [m.l, W - m.r])
  const gp = (a: number, d = 0) => Math.max(0, (aim - a) * Math.tan(((3 + d) * Math.PI) / 180)) * FT
  const ymax = Math.max(400, ...fin.map(q => q.agl * FT)) * 1.1
  const Y = linear([0, ymax], [H - m.b, m.t])
  return (
    <figure className="m-0">
      <figcaption className="caps mb-1">Final · against the 3° glidepath to the aim point ({fmt(aim)} m) · ±1°</figcaption>
      <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label="The final approach">
        {ticks(0, ymax, 5).map(a => (
          <g key={a}><line className="grid" x1={m.l} x2={W - m.r} y1={Y(a)} y2={Y(a)} /><text x={m.l - 6} y={Y(a) + 3} textAnchor="end">{a}</text></g>
        ))}
        {(W < 600 ? [0.5, 1, 2, 3] : [0.25, 0.5, 1, 1.5, 2, 2.5, 3]).filter(n => n * NM < far).map(n => (
          <g key={n}><line className="grid" x1={X(-n * NM)} x2={X(-n * NM)} y1={m.t} y2={H - m.b} /><text x={X(-n * NM)} y={H - m.b + 14} textAnchor="middle">{n} nm</text></g>
        ))}
        <rect x={X(0)} y={Y(0) - 3} width={X(Math.max(200, aim + 200)) - X(0)} height={4} fill="var(--line-2)" />
        <text x={X(0)} y={H - m.b + 14} textAnchor="middle">THR</text>
        {[-1, 1].map(d => <line key={d} x1={X(-far)} y1={Y(gp(-far, d))} x2={X(aim)} y2={Y(0)} stroke="var(--band-2)" strokeDasharray="3 5" />)}
        <line x1={X(-far)} y1={Y(gp(-far))} x2={X(aim)} y2={Y(0)} stroke="var(--band-0)" strokeWidth={1.3} />
        <polyline points={pts(fin.map(q => [X(q.along), Y(q.agl * FT)]))} fill="none" stroke="var(--datum)" strokeWidth={2.4} strokeLinejoin="round" />
        <circle cx={X(r.touchdown_from_threshold_m)} cy={Y(0)} r={4} fill="var(--wave)" />
      </svg>
    </figure>
  )
}

/** After the wheels are down: the rollout and the taxi, on the airfield and against the clock. */
export function TaxiPlots({ r, track }: { r: FieldLandingResult; track: TrackPt[] }) {
  const t = r.taxi
  const f = runwayFrame(r)
  const fast = track.filter(q => q.speed_kts > TAXI_LIMIT_KTS)
  // split the path where it crosses the taxi limit, so the fast parts show red
  const segs = useMemo(() => {
    const out: { id: string; pts: TrackPt[]; color: string }[] = []
    let cur: TrackPt[] = [], hot = false
    track.forEach((q, i) => {
      const h = q.speed_kts > TAXI_LIMIT_KTS && (q.t > (t?.rollout_s ?? 0))
      if (i && h !== hot) { out.push({ id: `s${out.length}`, pts: [...cur, q], color: hot ? 'var(--wave)' : 'var(--ball)' }); cur = [] }
      hot = h
      cur.push(q)
    })
    if (cur.length > 1) out.push({ id: `s${out.length}`, pts: cur, color: hot ? 'var(--wave)' : 'var(--ball)' })
    return out
  }, [track, t?.rollout_s])
  const W = usePlotW()
  if (track.length < 2) return null
  const at = (pred: (q: TrackPt) => boolean) => track.find(pred)
  const slowAt = at(q => q.t >= (t?.rollout_s ?? 0))
  const exitAt = t?.runway_clear_s != null ? at(q => q.t >= t.runway_clear_s!) : undefined
  const end = track[track.length - 1]
  const H = 170, m = { l: 44, r: 14, t: 12, b: 28 }
  const tMax = Math.max(1, end.t)
  const X = linear([0, tMax], [m.l, W - m.r])
  const vmax = Math.max(40, ...track.map(q => q.speed_kts)) * 1.1
  const Y = linear([0, vmax], [H - m.b, m.t])
  void f
  return (
    <div className="flex flex-col gap-3">
      <figure className="m-0">
        <figcaption className="caps mb-1">Rollout and taxi · on the airfield · red above {TAXI_LIMIT_KTS} kt</figcaption>
        <TrackMap
          height={W < 600 ? 300 : 380}
          imagery
          pad={250}
          lines={segs.map(s => ({ id: s.id, pts: s.pts, color: s.color }))}
          points={[
            { id: 'td', lat: track[0].lat, lon: track[0].lon, label: 'Touchdown', color: 'var(--wave)' },
            ...(slowAt ? [{ id: 'slow', lat: slowAt.lat, lon: slowAt.lon, label: W < 600 ? `${TAXI_LIMIT_KTS} kt` : `${TAXI_LIMIT_KTS} kt · ${fmt(t?.rollout_m ?? 0)} m`, color: 'var(--chalk)', shape: 'ring' as const, side: exitAt && exitAt.lon > slowAt.lon ? 'left' as const : 'right' as const }] : []),
            ...(exitAt ? [{ id: 'exit', lat: exitAt.lat, lon: exitAt.lon, label: `${W < 600 ? 'Clear' : 'Clear of the runway'} · ${fmt(t?.runway_clear_s ?? 0)} s`, color: 'var(--datum)', shape: 'diamond' as const }] : []),
            { id: 'end', lat: end.lat, lon: end.lon, label: t ? ({ parked: 'Parked', stopped_on_runway: W < 600 ? 'Stopped on rwy' : 'Stopped on the runway', left: W < 600 ? 'Left a/c' : 'Left the aircraft', took_off: 'Took off', timeout: 'Lost track' } as Record<string, string>)[t.end] ?? t.end : 'End', color: 'var(--sky)' },
          ]}
        />
      </figure>
      <figure className="m-0">
        <figcaption className="caps mb-1">Speed after touchdown · {fast.length ? `${fast.length} s over the taxi limit` : 'within the taxi limit'}</figcaption>
        <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label="Speed after touchdown">
          {ticks(0, vmax, 4).map(a => <g key={a}><line className="grid" x1={m.l} x2={W - m.r} y1={Y(a)} y2={Y(a)} /><text x={m.l - 6} y={Y(a) + 3} textAnchor="end">{a}</text></g>)}
          {ticks(0, tMax, W < 600 ? 5 : 8).map(s => <g key={`t${s}`}><line className="grid" x1={X(s)} x2={X(s)} y1={m.t} y2={H - m.b} /><text x={X(s)} y={H - m.b + 14} textAnchor="middle">{s}s</text></g>)}
          <line x1={m.l} x2={W - m.r} y1={Y(TAXI_LIMIT_KTS)} y2={Y(TAXI_LIMIT_KTS)} stroke="var(--wave)" strokeDasharray="4 4" />
          <text x={W - m.r - 4} y={Y(TAXI_LIMIT_KTS) - 4} textAnchor="end" style={{ fill: 'var(--wave)', fontSize: 10 }}>taxi limit {TAXI_LIMIT_KTS} kt</text>
          {t?.runway_clear_s != null && <line x1={X(t.runway_clear_s)} x2={X(t.runway_clear_s)} y1={m.t} y2={H - m.b} stroke="var(--datum)" strokeDasharray="2 3" />}
          <polyline points={pts(track.map(q => [X(q.t), Y(q.speed_kts)]))} fill="none" stroke="var(--ball)" strokeWidth={2} strokeLinejoin="round" />
          <text x={14} y={m.t} transform={`rotate(-90 14 ${m.t})`} textAnchor="end" className="axis-label">kt</text>
        </svg>
      </figure>
    </div>
  )
}
