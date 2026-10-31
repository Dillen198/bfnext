/**
 * Target plot in metres, north up, centred on the target (FunkMan style):
 * sand target, dotted scoring rings, grid, impacts coloured on the site's
 * score scale. One impact with `focus` gets the full debrief treatment (impact
 * and aircraft boxes, attack direction); many impacts get a CEP circle.
 */
import { useMemo, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { TONE_VAR, airframe, fmt, mToFt, qualityTone, weaponName } from '../../lib/format'
import { bombBands } from '../../lib/grading'
import type { BombQuality, BombResult } from '../../types'
import { niceCeil, ticks } from './scale'

export interface ImpactDot {
  id: string
  north_m: number
  east_m: number
  miss_m: number
  quality: BombQuality
  label?: string
}

export function BombPlot({
  impacts,
  rings,
  goodRadius = 25,
  focus,
  unitType,
  cep,
  mpi,
  linkable = false,
  size = 640,
}: {
  impacts: ImpactDot[]
  rings: number[]
  goodRadius?: number
  /** the single drop being debriefed */
  focus?: BombResult
  unitType?: string
  cep?: number | null
  /** mean point of impact of a group */
  mpi?: { north: number; east: number } | null
  linkable?: boolean
  size?: number
}) {
  const nav = useNavigate()
  const [hover, setHover] = useState<ImpactDot | null>(null)
  const W = size
  const H = size - 24
  const m = { l: 44, r: 34, t: 16, b: 38 }
  const plot = W - m.l - m.r

  const R = useMemo(() => {
    const misses = impacts.map(i => i.miss_m).sort((a, b) => a - b)
    // frame the bulk of the group; outliers get an edge arrow instead
    const p90 = misses.length ? misses[Math.min(misses.length - 1, Math.floor(misses.length * 0.9))] : 0
    const want = Math.max(goodRadius * 2.3, (focus ? focus.miss_m : p90) * 1.3, cep ? cep * 1.6 : 0, 30)
    return niceCeil(want)
  }, [impacts, goodRadius, focus, cep])
  const s = plot / (2 * R)
  const cx = m.l + plot / 2, cy = m.t + plot / 2
  const X = (e: number) => cx + e * s
  const Y = (n: number) => cy - n * s
  const clampEdge = (n: number, e: number) => {
    const k = Math.min(1, (R * 0.97) / Math.max(Math.abs(n), Math.abs(e), 1e-9))
    return { n: n * k, e: e * k, clipped: k < 1 }
  }
  const tks = ticks(-R, R, 8)
  const bands = bombBands(goodRadius).slice(1)

  const f = focus
  const hdg = f ? (f.release.heading_deg * Math.PI) / 180 : 0
  const cur = hover

  return (
    <div className="relative">
      <svg viewBox={`0 0 ${W} ${H}`} className="plot" role="img" aria-label={focus ? 'Bomb impact against the target' : 'Impact group on the target'}>
        <defs>
          <marker id="bp-arrow" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
            <path d="M0 0 10 5 0 10z" fill="var(--datum)" />
          </marker>
          <marker id="bp-north" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="8" markerHeight="8" orient="auto">
            <path d="M0 0 10 5 0 10z" fill="var(--chalk)" />
          </marker>
        </defs>
        <rect x={m.l} y={m.t} width={plot} height={plot} fill="none" stroke="var(--line-2)" />
        {tks.map(v => (
          <g key={v}>
            <line className={v === 0 ? 'grid-2' : 'grid'} x1={X(v)} x2={X(v)} y1={m.t} y2={m.t + plot} />
            <line className={v === 0 ? 'grid-2' : 'grid'} y1={Y(v)} y2={Y(v)} x1={m.l} x2={m.l + plot} />
            <text x={X(v)} y={m.t + plot + 14} textAnchor="middle">{v}</text>
            <text x={m.l - 6} y={Y(v) + 3} textAnchor="end">{v}</text>
          </g>
        ))}
        <text className="axis-label" x={m.l + plot} y={H - 6} textAnchor="end">metres east of the target</text>
        {/* target */}
        <circle cx={cx} cy={cy} r={Math.max(5, Math.min(4, goodRadius * 0.2) * s)} fill="var(--sand)" opacity={0.9} />
        {/* scoring bands */}
        {bands.map(b => (
          <g key={b.quality}>
            <circle cx={cx} cy={cy} r={b.r * s} fill="none" stroke={TONE_VAR[qualityTone(b.quality)]} strokeDasharray="2 4" strokeWidth={1.2} opacity={0.85} />
            <text x={cx + b.r * s * 0.7071 + 3} y={cy - b.r * s * 0.7071 - 3} style={{ fill: TONE_VAR[qualityTone(b.quality)], fontSize: 9 }}>{b.quality} {fmt(b.r)} m</text>
          </g>
        ))}
        {rings.filter(r => !bands.some(b => Math.abs(b.r - r) < 0.5) && r * s < plot / 2).map(r => (
          <circle key={`ring${r}`} cx={cx} cy={cy} r={r * s} fill="none" stroke="var(--dim)" strokeDasharray="1 5" />
        ))}
        {cep ? (
          <g>
            <circle cx={cx} cy={cy} r={cep * s} fill="var(--ball-soft)" stroke="var(--ball)" strokeWidth={1.6} strokeDasharray="7 4" />
            <text x={cx} y={cy - cep * s - 5} textAnchor="middle" style={{ fill: 'var(--ball)', fontWeight: 600 }}>CEP {fmt(cep, 1)} m</text>
          </g>
        ) : null}
        {mpi && (
          <g pointerEvents="none">
            <path d={`M ${X(mpi.east) - 7} ${Y(mpi.north) - 7} L ${X(mpi.east) + 7} ${Y(mpi.north) + 7} M ${X(mpi.east) + 7} ${Y(mpi.north) - 7} L ${X(mpi.east) - 7} ${Y(mpi.north) + 7}`} stroke="var(--ball)" strokeWidth={2.4} />
            <text x={X(mpi.east) + 10} y={Y(mpi.north) + 14} style={{ fill: 'var(--ball)' }}>MPI</text>
          </g>
        )}
        {/* attack direction */}
        {f && (
          <line
            x1={cx - Math.sin(hdg) * R * 0.9 * s} y1={cy + Math.cos(hdg) * R * 0.9 * s}
            x2={cx - Math.sin(hdg) * R * 0.3 * s} y2={cy + Math.cos(hdg) * R * 0.3 * s}
            stroke="var(--datum)" strokeWidth={2} markerEnd="url(#bp-arrow)"
          />
        )}
        {/* north */}
        <g transform={`translate(${m.l + plot + 17}, ${m.t + 2})`}>
          <line x1={0} y1={34} x2={0} y2={2} stroke="var(--chalk)" strokeWidth={1.6} markerEnd="url(#bp-north)" />
          <text x={0} y={48} textAnchor="middle" style={{ fill: 'var(--chalk)', fontWeight: 700 }}>N</text>
        </g>
        {/* impacts */}
        {impacts.map(i => {
          const p = clampEdge(i.north_m, i.east_m)
          const c = TONE_VAR[qualityTone(i.quality)]
          const big = !!focus
          return (
            <g
              key={i.id}
              style={{ cursor: linkable ? 'pointer' : undefined }}
              onPointerEnter={() => setHover(i)}
              onPointerLeave={() => setHover(h => (h?.id === i.id ? null : h))}
              onClick={linkable ? () => nav(`/result/${encodeURIComponent(i.id)}`) : undefined}
            >
              {p.clipped ? (
                <path d={`M ${X(p.e)} ${Y(p.n)} l -5 -9 l 10 0 z`} fill={c} transform={`rotate(${(Math.atan2(p.e, p.n) * 180) / Math.PI} ${X(p.e)} ${Y(p.n)})`} />
              ) : (
                <circle cx={X(p.e)} cy={Y(p.n)} r={big ? 7 : cur?.id === i.id ? 6 : 4.2} fill={c} stroke="var(--plot-bg)" strokeWidth={1.5} opacity={big ? 1 : 0.9} />
              )}
              <circle cx={X(p.e)} cy={Y(p.n)} r={11} fill="transparent" />
            </g>
          )
        })}
        {/* FunkMan boxes for a single drop: each in the plot corner on its
            own side (impact / approach), with a leader line, so neither
            ever covers the impact dot */}
        {f && (() => {
          const ix = X(f.impact_east_m), iy = Y(f.impact_north_m)
          const ax = cx - Math.sin(hdg) * R * 0.9 * s
          const ay = cy + Math.cos(hdg) * R * 0.9 * s
          const BW = 136, BH = 74, pad = 8
          const corner = (right: boolean, bottom: boolean) => ({
            x: right ? m.l + plot - BW - pad : m.l + pad,
            y: bottom ? m.t + plot - BH - pad : m.t + pad,
            right, bottom,
          })
          const ic = corner(ix >= cx, iy >= cy)
          let ac = corner(ax >= cx, ay >= cy)
          if (ac.right === ic.right && ac.bottom === ic.bottom) ac = corner(ac.right, !ac.bottom)
          // keep the north arrow's corner (top right) clear
          const box = (c: { x: number; y: number }, stroke: string, lines: [string, string?][], lx: number, ly: number) => (
            <g>
              <line x1={lx} y1={ly} x2={Math.max(c.x, Math.min(c.x + BW, lx))} y2={Math.max(c.y, Math.min(c.y + BH, ly))} stroke={stroke} strokeWidth={1} strokeDasharray="3 3" opacity={0.8} />
              <rect x={c.x} y={c.y} width={BW} height={BH} rx={2} fill="var(--panel)" stroke={stroke} />
              {lines.map(([t, col], i) => (
                <text key={i} x={c.x + 8} y={c.y + 16 + i * 15.5} style={{ fill: col ?? 'var(--chalk)', fontSize: i === 0 ? 11.5 : 11, fontWeight: i === 0 || col ? 600 : 400 }}>{t}</text>
              ))}
            </g>
          )
          const q = TONE_VAR[qualityTone(f.quality)]
          return (
            <g style={{ fontFamily: 'var(--font-mono)' }}>
              {box(ic, q, [[weaponName(f.weapon)], [`r=${fmt(f.miss_m, 1)} m`], [`φ=${fmt(f.radial_deg, 1)}°`], [f.quality, q]], ix, iy)}
              {box(ac, 'var(--datum)', [[airframe(unitType ?? '')], [`h=${fmt(mToFt(f.release.alt_agl_m))} ft`], [`v=${fmt(f.release.tas_kts)} kts`], [`ψ=${fmt(f.release.heading_deg)}°`]], ax, ay)}
              <circle cx={ix} cy={iy} r={7} fill={q} stroke="var(--plot-bg)" strokeWidth={1.5} />
            </g>
          )
        })()}
      </svg>
      {!focus && (
        <div className="readout">
          {cur ? (
            <>
              <span><b>{cur.label ?? cur.id}</b></span>
              <span>miss <b>{fmt(cur.miss_m, 1)} m</b></span>
              <span>N <b>{fmt(cur.north_m, 1)}</b> E <b>{fmt(cur.east_m, 1)}</b></span>
              <span style={{ color: TONE_VAR[qualityTone(cur.quality)] }}>{cur.quality}</span>
            </>
          ) : (
            <span>{impacts.length} impact{impacts.length === 1 ? '' : 's'} · hover a dot for details{linkable ? ', click to open its debrief' : ''}</span>
          )}
        </div>
      )}
    </div>
  )
}
