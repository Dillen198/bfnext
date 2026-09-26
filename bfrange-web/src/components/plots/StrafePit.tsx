/**
 * Strafe pass: the run-in in profile (height above the pit vs distance to
 * it) with the foul line, the closest range reached, and the accuracy
 * against the grading bands.
 */
import { TONE_VAR, fmt, qualityTone } from '../../lib/format'
import { SCORING_DEFAULTS } from '../../lib/grading'
import { distanceM } from '../../lib/geo'
import type { StrafeResult, TrackPt } from '../../types'
import { linear, ticks } from './scale'

const [DEADEYE, EXCELLENT, GOOD, INEFFECTIVE] = SCORING_DEFAULTS.strafe_bands
const BAND_SPANS = [
  { lo: 0, hi: INEFFECTIVE, q: 'POOR' },
  { lo: INEFFECTIVE, hi: GOOD, q: 'INEFFECTIVE' },
  { lo: GOOD, hi: EXCELLENT, q: 'GOOD' },
  { lo: EXCELLENT, hi: DEADEYE, q: 'EXCELLENT' },
  { lo: DEADEYE, hi: 100, q: 'DEADEYE' },
]

export function StrafePit({ r, path }: { r: StrafeResult; path?: TrackPt[] }) {
  const W = 900, H = 260, m = { l: 52, r: 20, t: 18, b: 34 }
  const pts = (path ?? []).map(p => ({ d: distanceM(r.target_pos, p), agl: p.alt_m - r.target_pos.alt_m }))
  // only the inbound leg: until the closest point
  let minI = 0
  pts.forEach((p, i) => { if (p.d < pts[minI].d) minI = i })
  const inbound = pts.slice(0, minI + 1)
  const outbound = pts.slice(minI)
  const maxD = Math.max(3000, ...inbound.map(p => p.d))
  const maxAlt = Math.max(600, ...pts.map(p => p.agl), r.entry_alt_agl_m) * 1.15
  const x = linear([maxD, 0], [m.l, W - m.r])
  const y = linear([0, maxAlt], [H - m.b, m.t])
  const fouled = r.foul_line_crossed
  const bands = SCORING_DEFAULTS.strafe_bands
  const acc = r.accuracy_pct
  const tone = TONE_VAR[qualityTone(r.quality)]

  return (
    <div className="flex flex-col gap-4">
      <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label="Strafe run-in profile">
        <rect x={x(r.foul_line_m)} y={m.t} width={x(0) - x(r.foul_line_m)} height={H - m.t - m.b} fill="var(--wave-soft)" />
        {ticks(0, maxD, 8).map(d => (
          <g key={d}>
            <line className="grid" x1={x(d)} x2={x(d)} y1={m.t} y2={H - m.b} />
            <text x={x(d)} y={H - m.b + 14} textAnchor="middle">{d}</text>
          </g>
        ))}
        {ticks(0, maxAlt, 5).map(a => (
          <g key={`a${a}`}>
            <line className="grid" x1={m.l} x2={W - m.r} y1={y(a)} y2={y(a)} />
            <text x={m.l - 6} y={y(a) + 3} textAnchor="end">{a}</text>
          </g>
        ))}
        <text className="axis-label" x={W - m.r} y={H - 4} textAnchor="end">metres to the pit</text>
        <text className="axis-label" x={14} y={m.t} transform={`rotate(-90 14 ${m.t})`} textAnchor="end">m above the pit</text>
        <line x1={x(r.foul_line_m)} x2={x(r.foul_line_m)} y1={m.t} y2={H - m.b} stroke="var(--wave)" strokeDasharray="6 4" strokeWidth={1.5} />
        <text x={x(r.foul_line_m) - 6} y={m.t + 12} textAnchor="end" style={{ fill: 'var(--wave)' }}>FOUL LINE {fmt(r.foul_line_m)} m</text>
        {inbound.length > 1 && (
          <polyline points={inbound.map(p => `${x(p.d)},${y(p.agl)}`).join(' ')} fill="none" stroke="var(--sky)" strokeWidth={2.2} />
        )}
        {outbound.length > 1 && (
          <polyline points={outbound.map(p => `${x(Math.max(0, p.d))},${y(p.agl)}`).join(' ')} fill="none" stroke="var(--sky)" strokeWidth={1.4} strokeDasharray="3 4" opacity={0.6} />
        )}
        <line x1={x(r.min_range_m)} x2={x(r.min_range_m)} y1={H - m.b} y2={H - m.b - 26} stroke={fouled ? 'var(--wave)' : 'var(--datum)'} strokeWidth={3} />
        <text x={x(r.min_range_m)} y={H - m.b - 30} textAnchor="middle" style={{ fill: fouled ? 'var(--wave)' : 'var(--datum)', fontWeight: 600 }}>
          closest {fmt(r.min_range_m)} m
        </text>
        <rect x={x(0) - 5} y={y(0) - 14} width={10} height={14} fill="var(--sand)" />
        <text x={x(0) - 8} y={y(0) - 18} textAnchor="end" style={{ fill: 'var(--chalk)' }}>{r.pit}</text>
      </svg>
      <div>
        <div className="flex items-baseline gap-3 mb-2">
          <span className="caps">Accuracy</span>
          <span className="num text-[22px]" style={{ color: tone }}>{fmt(acc, 1)}%</span>
          <span className="muted mono text-[12px]">{r.hits} / {r.rounds_fired} rounds · {r.gun}</span>
        </div>
        <svg viewBox="0 0 900 44" className="plot" style={{ background: 'transparent' }} role="img" aria-label={`Accuracy ${fmt(acc, 1)} percent`}>
          {BAND_SPANS.map(({ lo, hi, q }) => (
            <rect key={q} x={lo * 9} y={8} width={(hi - lo) * 9} height={14} fill={TONE_VAR[qualityTone(q)]} opacity={0.35} />
          ))}
          {bands.map(b => (
            <text key={b} x={b * 9} y={38} textAnchor="middle">{b}%</text>
          ))}
          <rect x={0} y={8} width={Math.max(2, acc * 9)} height={14} fill={tone} />
          <line x1={acc * 9} x2={acc * 9} y1={2} y2={28} stroke="var(--chalk)" strokeWidth={2} />
        </svg>
        {fouled && <div className="chip bad mt-2">{r.invalid_reason ?? 'Crossed the foul line: pass scored invalid'}</div>}
      </div>
    </div>
  )
}
