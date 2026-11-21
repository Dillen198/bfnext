/**
 * The drawn half of a debrief, rebuilt in the browser from the record's
 * sampled `track`, so it can be hovered, scrubbed and replayed.
 */
import { Empty } from '../../components/States'
import { AarPlots } from '../../components/plots/AarPlots'
import { BombPlot } from '../../components/plots/BombPlot'
import { GatePlot } from '../../components/plots/GatePlot'
import { InterceptReplay } from '../../components/plots/InterceptReplay'
import { StrafePit } from '../../components/plots/StrafePit'
import { RunwayPlot } from '../../components/plots/RunwayPlot'
import { FinalProfile, PatternPlan, PatternProfile, TaxiPlots } from '../../components/plots/FieldPlots'
import { PathReplay, TrackMap } from '../../components/plots/TrackMap'
import { TrapSheet } from '../../components/plots/TrapSheet'
import { TONE_VAR, fmt, qualityTone } from '../../lib/format'
import { toLocal } from '../../lib/geo'
import type { GeoPt, PrecisionQuality, RangeRecord } from '../../types'

/** Default "perfect" radii from bfprotocols cfg.rs (a server may override them). */
const PERFECT = { landing: 3, deck: 1.5, sling: 5, airdrop: 50 }

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
          <PrecisionPlot
            centre={r.pad_pos} at={r.touchdown_pos} perfect={r.drill === 'ship' ? PERFECT.deck : PERFECT.landing} quality={r.quality}
            label={r.drill === 'ship' ? `Touchdown against the deck spot${r.ship_speed_kts !== undefined ? ` · ship at ${fmt(r.ship_speed_kts)} kt` : ''}` : 'Touchdown against the pad'}
          />
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
    case 'field_landing': {
      // the engine writes the pattern and the taxi as two paths; an older record has one path for the approach
      const paths = t?.kind === 'path' ? t.paths : {}
      const pattern = paths.pattern ?? Object.values(paths).find(v => v.length && v[v.length - 1].t <= 0.5) ?? []
      const taxi = paths.taxi ?? []
      return (
        <div className="flex flex-col gap-5">
          {pattern.length > 4 && <PatternPlan r={r} track={pattern} />}
          {pattern.length > 4 && <PatternProfile r={r} track={pattern} />}
          {pattern.length > 4 && <FinalProfile r={r} track={pattern} />}
          <RunwayPlot r={r} />
          {taxi.length > 1 ? <TaxiPlots r={r} track={taxi} /> : r.outcome === 'touch_and_go'
            ? <p className="muted text-[12.5px] m-0">A touch-and-go: no rollout or taxi to debrief.</p>
            : <p className="muted text-[12.5px] m-0">No taxi recorded for this landing (an older record, or the pilot left the aircraft on the runway).</p>}
        </div>
      )
    }
    case 'low_level':
      return r.gates.length || t?.kind === 'path' ? (
        <div className="flex flex-col gap-4">
          <GatePlot r={r} />
          {t?.kind === 'path' && <PathReplay paths={t.paths} height={380} />}
        </div>
      ) : none
    case 'sead': {
      const path = t?.kind === 'path' ? Object.values(t.paths)[0] ?? [] : []
      return (
        <figure className="m-0">
          <figcaption className="caps mb-1">Your flight and the site{r.launch_range_m !== null ? ` · fired from ${fmt(r.launch_range_m / 1852, 1)} nm` : ''}</figcaption>
          <TrackMap
            height={420}
            lines={path.length > 1 ? [{ id: 'you', pts: path, color: 'var(--ball)' }] : []}
            points={[
              ...(path.length ? [{ id: 'start', lat: path[0].lat, lon: path[0].lon, label: rec.pilot.name, color: 'var(--ball)', shape: 'ring' as const }] : []),
              { id: 'site', lat: r.site_pos.lat, lon: r.site_pos.lon, label: `${r.site} · ${r.system}${r.site_destroyed ? ' · destroyed' : ''}`, color: r.site_destroyed ? 'var(--datum)' : 'var(--wave)', shape: 'diamond' },
            ]}
          />
        </figure>
      )
    }
    case 'hot_zone':
      return (
        <div className="flex flex-col gap-4">
          <figure className="m-0">
            <figcaption className="caps mb-2">Kills</figcaption>
            {r.kills.length === 0 ? (
              <div className="muted text-[13px]">No kills this sortie.</div>
            ) : (
              <div className="flex flex-wrap gap-2">
                {r.kills.map((k, i) => (
                  <div key={`${i}-${k}`} className="grid place-items-center mono text-[11px] px-2" style={{ minWidth: 64, height: 40, borderRadius: 2, border: '1px solid var(--datum)', background: 'var(--datum-soft)', color: 'var(--datum)' }}>
                    {k}
                  </div>
                ))}
              </div>
            )}
          </figure>
          {t?.kind === 'path' && <PathReplay paths={t.paths} height={380} />}
        </div>
      )
    case 'airdrop':
      return r.method === 'airland' ? (
        <Empty title="An air-land delivery">{`${r.cargo} unloaded at ${r.zone}: nothing to plot.`}</Empty>
      ) : (
        <div className="flex flex-col gap-4">
          <PrecisionPlot centre={r.dz_pos} at={r.impact_pos} perfect={PERFECT.airdrop} quality={r.quality} label={`Where the load came down against ${r.zone} DZ`} />
          {r.release && (
            <TrackMap
              height={340}
              imagery
              lines={[{ id: 'drift', pts: [r.release.pos, r.impact_pos], color: 'var(--ball)' }]}
              points={[
                { id: 'rel', lat: r.release.pos.lat, lon: r.release.pos.lon, label: `release · ${Math.round(r.release.alt_agl_ft)} ft`, color: 'var(--sky)', shape: 'ring' },
                { id: 'pi', lat: r.dz_pos.lat, lon: r.dz_pos.lon, label: `${r.zone} PI`, color: 'var(--chalk)', shape: 'diamond' },
                { id: 'imp', lat: r.impact_pos.lat, lon: r.impact_pos.lon, label: `came down · ${Math.round(r.distance_m)} m`, color: 'var(--ball)' },
              ]}
            />
          )}
        </div>
      )
    case 'csar': {
      const path = t?.kind === 'path' ? Object.values(t.paths)[0] ?? [] : []
      return (
        <figure className="m-0">
          <figcaption className="caps mb-1">Your flight and where the survivor was</figcaption>
          <TrackMap
            height={420}
            lines={path.length > 1 ? [{ id: 'you', pts: path, color: 'var(--sky)' }] : []}
            points={[
              ...(path.length ? [{ id: 'start', lat: path[0].lat, lon: path[0].lon, label: rec.pilot.name, color: 'var(--sky)', shape: 'ring' as const }] : []),
              { id: 'surv', lat: r.survivor_pos.lat, lon: r.survivor_pos.lon, label: `survivor · ${r.area}`, color: '#7CFC4A', shape: 'diamond' },
            ]}
          />
        </figure>
      )
    }
  }
}
