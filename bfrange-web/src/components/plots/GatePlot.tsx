/**
 * A low-level route gate by gate: the height at each gate against the
 * ceiling, and the time at each gate against the plan (+ = late).
 */
import { fmt, fmtSigned } from '../../lib/format'
import type { LowLevelResult } from '../../types'
import { linear, niceCeil, ticks } from './scale'

const timeTone = (e: number) => (Math.abs(e) <= 15 ? 'var(--datum)' : Math.abs(e) <= 45 ? 'var(--ball)' : 'var(--wave)')

export function GatePlot({ r }: { r: LowLevelResult }) {
  const W = 900, m = { l: 52, r: 16 }
  const n = Math.max(1, r.gates.length)
  const slot = (W - m.l - m.r) / n
  const gx = (i: number) => m.l + slot * (i + 0.5)
  const bw = Math.min(40, slot * 0.5)
  // heights
  const HA = 220, ta = 14, ba = 22
  const top = niceCeil(Math.max(100, r.max_allowed_agl_ft * 1.5, ...r.gates.map(g => g.agl_ft ?? 0)))
  const ya = linear([0, top], [HA - ba, ta])
  // timing
  const HT = 190, tt = 14, bt = 26
  const errs = r.gates.map(g => (g.t === null ? null : g.t - g.planned_t))
  const span = Math.max(10, ...errs.map(e => Math.abs(e ?? 0))) * 1.15
  const yt = linear([-span, span], [HT - bt, tt])
  if (!r.gates.length) return null
  return (
    <div className="flex flex-col gap-3">
      <figure className="m-0">
        <figcaption className="caps mb-1">Height at each gate · ft AGL</figcaption>
        <svg viewBox={`0 0 ${W} ${HA}`} className="plot" role="img" aria-label="Height at each gate against the ceiling">
          {ticks(0, top, 4).map(a => (
            <g key={a}>
              <line className="grid" x1={m.l} x2={W - m.r} y1={ya(a)} y2={ya(a)} />
              <text x={m.l - 6} y={ya(a) + 3} textAnchor="end">{a}</text>
            </g>
          ))}
          <line x1={m.l} x2={W - m.r} y1={ya(r.max_allowed_agl_ft)} y2={ya(r.max_allowed_agl_ft)} stroke="var(--wave)" strokeDasharray="6 4" strokeWidth={1.5} />
          <text x={m.l + 4} y={ya(r.max_allowed_agl_ft) - 5} style={{ fill: 'var(--wave)', fontWeight: 700 }}>ceiling {fmt(r.max_allowed_agl_ft)} ft</text>
          {r.gates.map((g, i) => {
            if (g.t === null) return <text key={g.gate} x={gx(i)} y={ya(top / 2)} textAnchor="middle" style={{ fill: 'var(--wave)', fontWeight: 700 }}>MISSED</text>
            if (g.agl_ft === null) return null
            const c = g.agl_ft <= r.max_allowed_agl_ft ? 'var(--datum)' : 'var(--wave)'
            return (
              <g key={g.gate}>
                <rect x={gx(i) - bw / 2} y={ya(g.agl_ft)} width={bw} height={ya(0) - ya(g.agl_ft)} fill={c} rx={2} />
                <text x={gx(i)} y={ya(g.agl_ft) - 4} textAnchor="middle" style={{ fill: c }}>{fmt(g.agl_ft)}</text>
              </g>
            )
          })}
          {r.gates.map((g, i) => <text key={`l${g.gate}`} x={gx(i)} y={HA - 6} textAnchor="middle">{g.gate}</text>)}
        </svg>
      </figure>
      <figure className="m-0">
        <figcaption className="caps mb-1">Timing at each gate · s against the plan, + = late</figcaption>
        <svg viewBox={`0 0 ${W} ${HT}`} className="plot" role="img" aria-label="Timing at each gate against the plan">
          {ticks(-span, span, 4).map(v => (
            <g key={v}>
              <line className="grid" x1={m.l} x2={W - m.r} y1={yt(v)} y2={yt(v)} />
              <text x={m.l - 6} y={yt(v) + 3} textAnchor="end">{v > 0 ? `+${v}` : v}</text>
            </g>
          ))}
          <line x1={m.l} x2={W - m.r} y1={yt(0)} y2={yt(0)} stroke="var(--haze)" />
          {r.gates.map((g, i) => {
            const e = errs[i]
            if (e === null) return <text key={g.gate} x={gx(i)} y={yt(0) - 6} textAnchor="middle" style={{ fill: 'var(--wave)', fontWeight: 700 }}>MISSED</text>
            const c = timeTone(e)
            const y0 = yt(Math.max(0, e)), y1 = yt(Math.min(0, e))
            return (
              <g key={g.gate}>
                <rect x={gx(i) - bw / 2} y={y0} width={bw} height={Math.max(2, y1 - y0)} fill={c} rx={2} />
                <text x={gx(i)} y={e >= 0 ? y0 - 4 : y1 + 12} textAnchor="middle" style={{ fill: c }}>{fmtSigned(e, 0)}</text>
              </g>
            )
          })}
          {r.gates.map((g, i) => <text key={`l${g.gate}`} x={gx(i)} y={HT - 8} textAnchor="middle">{g.gate}</text>)}
        </svg>
      </figure>
    </div>
  )
}
