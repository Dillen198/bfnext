/**
 * The drawn half of a debrief, rebuilt in the browser from the record's
 * sampled `track`, so it can be hovered, scrubbed and replayed.
 */
import { Empty } from '../../components/States'
import { AarPlots } from '../../components/plots/AarPlots'
import { BombPlot } from '../../components/plots/BombPlot'
import { InterceptReplay } from '../../components/plots/InterceptReplay'
import { StrafePit } from '../../components/plots/StrafePit'
import { PathReplay, TrackMap } from '../../components/plots/TrackMap'
import { TrapSheet } from '../../components/plots/TrapSheet'
import { TONE_VAR, fmt, qualityTone } from '../../lib/format'
import { toLocal } from '../../lib/geo'
import type { GeoPt, PrecisionQuality, RangeRecord } from '../../types'

/** Default "perfect" radii from bfprotocols cfg.rs (a server may override them). */
const PERFECT = { landing: 3, sling: 5 }

/** Where a helicopter set down against its pad / drop zone. */
function PrecisionPlot({ centre, at, perfect, quality, label }: { centre: GeoPt; at: GeoPt; perfect: number; quality: PrecisionQuality; label: string }) {
  const W = 420, c = W / 2
  const R = perfect * 9
  const s = (c - 20) / R
  const p = toLocal(centre, at)
  const bands: [PrecisionQuality, number][] = [['PERFECT', perfect], ['EXCELLENT', perfect * 2], ['GOOD', perfect * 4], ['FAIR', perfect * 8]]
  const d = Math.hypot(p.north, p.east)
  const clampK = Math.min(1, (R * 0.95) / Math.max(d, 1e-9))
  return (
    <figure className="m-0">
      <figcaption className="caps mb-1">{label} · default bands (PERFECT ≤ {perfect} m)</figcaption>
      <svg viewBox={`0 0 ${W} ${W}`} className="plot" style={{ maxWidth: 420 }} role="img" aria-label={`${label}: ${fmt(d, 1)} metres off`}>
        {[...bands].reverse().map(([q, r]) => (
          <g key={q}>
            <circle cx={c} cy={c} r={r * s} fill="none" stroke={TONE_VAR[qualityTone(q)]} strokeDasharray="2 4" />
            <text x={c + r * s * 0.7071 + 3} y={c - r * s * 0.7071 - 2} style={{ fill: TONE_VAR[qualityTone(q)], fontSize: 9 }}>{q} {r} m</text>
          </g>
        ))}
        <line className="grid-2" x1={c} x2={c} y1={10} y2={W - 10} />
        <line className="grid-2" y1={c} y2={c} x1={10} x2={W - 10} />
        <circle cx={c} cy={c} r={4} fill="var(--sand)" />
        <circle cx={c + p.east * s * clampK} cy={c - p.north * s * clampK} r={7} fill={TONE_VAR[qualityTone(quality)]} stroke="var(--plot-bg)" strokeWidth={2} />
        <text x={W - 12} y={20} textAnchor="end" style={{ fill: 'var(--chalk)', fontWeight: 700 }}>N ↑</text>
      </svg>
    </figure>
  )
}

export function Interactive({ rec }: { rec: RangeRecord }) {
  const r = rec.result
  const t = rec.track
  const none = <Empty title="No track recorded">This result was saved without sampled geometry (older records lose their tracks after a while).</Empty>

  switch (r.kind) {
    case 'trap':
      return t?.kind === 'groove' && t.samples.length > 1 ? <TrapSheet samples={t.samples} trap={r} /> : none
    case 'bomb': {
      const pts = t?.kind === 'weapon' ? t.points : []
      return (
        <div className="flex flex-col gap-4">
          <BombPlot
            impacts={[{ id: rec.id, north_m: r.impact_north_m, east_m: r.impact_east_m, miss_m: r.miss_m, quality: r.quality }]}
            rings={r.rings_m}
            goodRadius={r.good_radius_m || undefined}
            focus={r}
            unitType={rec.unit_type}
          />
          <figure className="m-0">
            <figcaption className="caps mb-1">Release to impact</figcaption>
            <TrackMap
              height={340}
              lines={[
                ...(pts.length > 1 ? [{ id: 'weapon', pts, color: 'var(--wave)' }] : [{ id: 'weapon', pts: [r.release.pos, r.impact], color: 'var(--wave)', dashed: true }]),
              ]}
              points={[
                { id: 'rel', lat: r.release.pos.lat, lon: r.release.pos.lon, label: `release · ${fmt(r.release.ground_range_m)} m`, color: 'var(--datum)', shape: 'ring' },
                { id: 'tgt', lat: r.target_pos.lat, lon: r.target_pos.lon, label: r.target, color: 'var(--sand)', shape: 'diamond' },
                { id: 'imp', lat: r.impact.lat, lon: r.impact.lon, label: `impact · ${fmt(r.miss_m, 1)} m`, color: TONE_VAR[qualityTone(r.quality)] },
              ]}
            />
          </figure>
        </div>
      )
    }
    case 'strafe':
      return <StrafePit r={r} path={t?.kind === 'path' ? Object.values(t.paths)[0] : undefined} />
    case 'aar':
      return t?.kind === 'aar' && t.samples.length > 1 ? <AarPlots samples={t.samples} r={r} /> : none
    case 'missile':
      return t?.kind === 'intercept' && t.missile.length > 1 ? <InterceptReplay missile={t.missile} target={t.target} r={r} /> : none
    case 'anti_ship': {
      const pts = t?.kind === 'weapon' ? t.points : [r.launch_pos, r.ship_pos]
      return (
        <TrackMap
          height={420}
          lines={[{ id: 'w', pts, color: 'var(--wave)', dashed: t?.kind !== 'weapon' }]}
          points={[
            { id: 'l', lat: r.launch_pos.lat, lon: r.launch_pos.lon, label: `launch · ${fmt(r.launch_range_m / 1852, 1)} nm`, color: 'var(--datum)', shape: 'ring' },
            { id: 's', lat: r.ship_pos.lat, lon: r.ship_pos.lon, label: `${r.ship}${r.hit ? ' · HIT' : ''}`, color: r.hit ? 'var(--datum)' : 'var(--wave)', shape: 'diamond' },
          ]}
        />
      )
    }
    case 'sling':
      return (
        <div className="flex flex-col gap-4">
          <PrecisionPlot centre={r.dz_pos} at={r.set_down_pos} perfect={PERFECT.sling} quality={r.quality} label="Set-down against the drop zone" />
          {t?.kind === 'path' && <PathReplay paths={t.paths} height={340} />}
        </div>
      )
    case 'landing':
      return (
        <div className="flex flex-col gap-4">
          <PrecisionPlot centre={r.pad_pos} at={r.touchdown_pos} perfect={PERFECT.landing} quality={r.quality} label="Touchdown against the pad" />
          {t?.kind === 'path' && <PathReplay paths={t.paths} height={340} />}
        </div>
      )
    case 'gunnery':
      return (
        <figure className="m-0">
          <figcaption className="caps mb-2">Targets</figcaption>
          <div className="flex flex-wrap gap-2">
            {Array.from({ length: r.targets_total }, (_, i) => {
              const dead = i < r.targets_killed
              return (
                <div key={i} className="grid place-items-center mono text-[11px]" style={{ width: 52, height: 40, borderRadius: 2, border: `1px solid ${dead ? 'var(--datum)' : 'var(--line-2)'}`, background: dead ? 'var(--datum-soft)' : 'transparent', color: dead ? 'var(--datum)' : 'var(--dim)' }}>
                  {dead ? 'KILL' : 'ALIVE'}
                </div>
              )
            })}
          </div>
          {t?.kind === 'path' && <div className="mt-4"><PathReplay paths={t.paths} height={340} /></div>}
        </figure>
      )
    case 'engagement':
    case 'troops':
    case 'cas':
      return t?.kind === 'path' ? <PathReplay paths={t.paths} /> : none
  }
}
