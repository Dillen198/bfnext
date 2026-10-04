/**
 * A runway landing from above: the threshold, the aim-point markings and
 * where the wheels touched, landing left to right (so right of the
 * centreline is down the page). The lateral scale is stretched so a few
 * metres off the centreline still shows.
 */
import { TONE_VAR, fmt, qualityTone } from '../../lib/format'
import type { FieldLandingResult } from '../../types'
import { linear, ticks } from './scale'

export function RunwayPlot({ r }: { r: FieldLandingResult }) {
  const W = 900, H = 250, m = { l: 16, r: 16, t: 26, b: 34 }
  const td = r.touchdown_from_threshold_m
  const aim = td - r.aim_error_m
  const lo = Math.min(0, td) - 250
  const hi = Math.max(1500, Math.max(td, aim) + 500)
  const x = linear([lo, hi], [m.l, W - m.r])
  const half = Math.max(35, Math.abs(r.centreline_m) * 1.4)
  const y = linear([-half, half], [m.t, H - m.b])
  const tone = r.outcome === 'undershoot' ? 'var(--wave)' : TONE_VAR[qualityTone(r.quality)]
  const tx = Math.min(W - m.r - 6, Math.max(m.l + 6, x(td)))
  const ty = Math.min(H - m.b - 6, Math.max(m.t + 6, y(r.centreline_m)))
  const right = tx > W * 0.7
  const label = `${fmt(Math.abs(r.aim_error_m))} m ${r.aim_error_m >= 0 ? 'long' : 'short'} · ${fmt(Math.abs(r.centreline_m), 1)} m ${r.centreline_m >= 0 ? 'right' : 'left'}`
  return (
    <figure className="m-0">
      <figcaption className="caps mb-1">Touchdown · top view, lateral scale stretched</figcaption>
      <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label={`Touchdown ${label}`}>
        {/* runway, 45 m wide, and its centreline */}
        <rect x={x(0)} y={y(-22.5)} width={W - m.r - x(0)} height={y(22.5) - y(-22.5)} fill="var(--plot-grid)" stroke="var(--plot-grid-2)" />
        <line x1={x(0)} x2={W - m.r} y1={y(0)} y2={y(0)} stroke="var(--haze)" strokeDasharray="14 10" />
        {[-18, -12, -6, 0, 6, 12, 18].map(b => (
          <line key={b} x1={x(0) + 4} x2={x(0) + 28} y1={y(b)} y2={y(b)} stroke="var(--chalk)" strokeWidth={3} />
        ))}
        {[-11, 11].map(b => <rect key={b} x={x(aim) - 12} y={y(b) - 4} width={24} height={8} fill="var(--chalk)" rx={1} />)}
        <line x1={x(aim)} x2={x(aim)} y1={m.t - 8} y2={H - m.b} stroke="var(--haze)" strokeDasharray="2 4" />
        <text x={x(aim)} y={m.t - 12} textAnchor="middle">aim point {fmt(aim)} m</text>
        <text x={x(0) + 2} y={m.t - 12}>threshold</text>
        {ticks(lo, hi, 7).map(d => (
          <text key={d} x={x(d)} y={H - m.b + 16} textAnchor="middle">{Object.is(d, -0) ? 0 : d}</text>
        ))}
        <text className="axis-label" x={W - m.r} y={H - 4} textAnchor="end">metres past the threshold</text>
        {/* the approach and the touchdown */}
        <line x1={m.l + 6} x2={Math.max(m.l + 30, tx - 14)} y1={ty} y2={ty} stroke="var(--datum)" strokeWidth={2} markerEnd="url(#rw-arrow)" />
        <defs>
          <marker id="rw-arrow" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto">
            <path d="M0 0L10 5L0 10z" fill="var(--datum)" />
          </marker>
        </defs>
        <circle cx={tx} cy={ty} r={8} fill={tone} stroke="var(--plot-bg)" strokeWidth={2} />
        <text x={right ? tx - 14 : tx + 14} y={ty > y(0) ? ty + 18 : ty - 10} textAnchor={right ? 'end' : 'start'} style={{ fill: tone, fontWeight: 700, fontSize: 12 }}>{label}</text>
      </svg>
    </figure>
  )
}
