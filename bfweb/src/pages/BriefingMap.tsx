import { useMemo } from 'react'
import Map, { Layer, Marker, Source } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useQuery } from '@tanstack/react-query'
import {
  api,
  type Frontlines,
  type Hotspot,
  type MapObjective,
  type SituationReport,
  type Task,
} from '../api'
import { useTheme } from '../context/ThemeContext'
import { Explosion } from '@icons'

// Inline raster style off the Esri canvas tiles — a hosted vector style is
// blocked by the dashboard's prod CSP and renders black. Same style the tactical
// scope uses, so the two maps read as one system.
const mapStyleFor = (theme: string) => ({
  version: 8 as const,
  sources: {
    esri: {
      type: 'raster' as const,
      tiles: [
        theme === 'light'
          ? 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
          : 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}',
      ],
      tileSize: 256,
      attribution: 'Esri',
    },
  },
  layers: [
    {
      id: 'bg',
      type: 'background' as const,
      paint: { 'background-color': theme === 'light' ? '#dfe0d8' : '#0a0d07' },
    },
    {
      id: 'esri',
      type: 'raster' as const,
      source: 'esri',
      paint: { 'raster-opacity': theme === 'light' ? 0.95 : 0.85 },
    },
  ],
})

// Same palette the F10 map uses (bflib/src/mapcolor.rs): pure red is
// unreadable over desert terrain, so Red is violet and Blue is azure. Keeping
// the two maps in step matters more than matching DCS's stock colours.
const OWNER_COLOR = (owner: string) =>
  owner === 'Blue' ? '#40a6ff' : owner === 'Red' ? '#b84dff' : '#ffffff'

/** Status buckets, identical to the engine's: >66 good, 33-66 warn, else bad. */
const BUCKET = (v: number) => (v > 66 ? '#33d94f' : v > 33 ? '#ffb300' : '#f22a2a')
const GOLD = '#ffd24a'

/**
 * The four status hexes -- health, logi, supply, fuel, in that fixed order,
 * exactly as they are drawn beside an objective on the F10 map. A gold outline
 * means that resource is unlimited.
 */
function StatusHexes({ o }: { o: MapObjective }) {
  const stats: [string, number | undefined, boolean][] = [
    ['health', o.health, false],
    ['logi', o.logi, false],
    ['supply', o.supply, !!o.unlimited_supply],
    ['fuel', o.fuel, false],
  ]
  return (
    <span style={{ display: 'flex', gap: 2, pointerEvents: 'none' }}>
      {stats.map(([label, v, unlimited]) =>
        v == null ? null : (
          <svg key={label} width={8} height={9} viewBox="0 0 20 22" aria-hidden>
            <title>{`${label} ${v}%${unlimited ? ' (unlimited)' : ''}`}</title>
            <polygon
              points="10,0 20,5.5 20,16.5 10,22 0,16.5 0,5.5"
              fill={BUCKET(v)}
              stroke={unlimited ? GOLD : 'rgba(0,0,0,0.8)'}
              strokeWidth={unlimited ? 3 : 1.5}
            />
          </svg>
        ),
      )}
    </span>
  )
}

const URGENCY_COLOR = (u: Task['urgency']) =>
  u === 'critical' ? '#f04747' : u === 'high' ? '#f0a030' : '#8ec83f'

/** A circle of `n` points at `radius_m` around a lat/lon, as a GeoJSON ring. */
function ringAround(lat: number, lon: number, radiusM: number, n = 48): number[][] {
  const latPerM = 1 / 110_574
  const lonPerM = 1 / (111_320 * Math.cos((lat * Math.PI) / 180) || 1)
  const pts: number[][] = []
  for (let i = 0; i <= n; i++) {
    const a = (i / n) * Math.PI * 2
    pts.push([lon + Math.cos(a) * radiusM * lonPerM, lat + Math.sin(a) * radiusM * latPerM])
  }
  return pts
}

interface Props {
  report: SituationReport
  /** Task the operator is hovering//selecting in the list, highlighted here. */
  selectedTaskId: string | null
  onSelectTask: (id: string | null) => void
}

/**
 * The briefing map: the coalition's own picture, laid over the territory.
 *
 * Every layer comes from the report, which the engine already built for one
 * side — the threat rings are the intel *this* coalition has earned, not
 * ground truth, so nothing here can leak the other side's positions.
 */
export default function BriefingMap({ report, selectedTaskId, onSelectTask }: Props) {
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])

  // The frontline is public geometry (never an objective position) and is the
  // one piece of context the report itself doesn't carry.
  const { data: fronts = { mid: [], blue: [], red: [] } } = useQuery<Frontlines>({
    queryKey: ['frontline'],
    queryFn: () => api.frontline(),
    refetchInterval: 60_000,
  })

  const positioned = useMemo(
    () => report.map.filter((o) => o.lat !== 0 || o.lon !== 0),
    [report.map],
  )

  // report.map carries a bare `threatened` flag; the matching hotspot record
  // is the one that knows *how* bad it is (an enemy capture timer actually
  // running vs. hostiles merely nearby). Join them by name so the marker can
  // show the difference.
  // NB: a plain record, not a Map -- `Map` is the react-map-gl component in
  // this file's scope.
  const hotspotByName = useMemo(() => {
    const m: Record<string, Hotspot | undefined> = {}
    for (const h of report.hotspots) m[h.objective] = h
    return m
  }, [report.hotspots])

  // Centre on the campaign, not on the world.
  const initialViewState = useMemo(() => {
    if (!positioned.length) return { latitude: 0, longitude: 0, zoom: 3 }
    const lat = positioned.reduce((s, o) => s + o.lat, 0) / positioned.length
    const lon = positioned.reduce((s, o) => s + o.lon, 0) / positioned.length
    return { latitude: lat, longitude: lon, zoom: 6.2 }
  }, [positioned])

  const frontGeo = useMemo(
    () => ({
      type: 'FeatureCollection' as const,
      features: (['mid', 'blue', 'red'] as const).flatMap((k) =>
        (fronts[k] ?? []).map((line) => ({
          type: 'Feature' as const,
          properties: { kind: k },
          geometry: {
            type: 'LineString' as const,
            coordinates: line.map(([lat, lon]) => [lon, lat]),
          },
        })),
      ),
    }),
    [fronts],
  )

  // Threat rings: only the ones the engine could size. An unidentified emitter
  // gets a marker (below) but no ring — drawing a guessed radius would be worse
  // than drawing none.
  const threatGeo = useMemo(
    () => ({
      type: 'FeatureCollection' as const,
      features: report.threats
        .filter((t) => (t.radius_m ?? 0) > 0)
        .map((t) => ({
          type: 'Feature' as const,
          properties: { confidence: t.confidence },
          geometry: {
            type: 'Polygon' as const,
            coordinates: [ringAround(t.lat, t.lon, t.radius_m!)],
          },
        })),
    }),
    [report.threats],
  )

  // NB: `Map` in this module is react-map-gl's component, so index the tasking
  // with a plain record rather than a JS Map.
  const taskIdx: Record<string, number> = {}
  report.tasking.forEach((t, i) => { taskIdx[t.id] = i + 1 })

  return (
    <Map
      key={theme}
      mapStyle={mapStyle}
      style={{ width: '100%', height: '100%' }}
      initialViewState={initialViewState}
      dragRotate={false}
      attributionControl={false}
      onClick={() => onSelectTask(null)}
    >
      {/* ── Frontline ───────────────────────────────────────────── */}
      <Source id="fronts" type="geojson" data={frontGeo}>
        <Layer
          id="fronts-line"
          type="line"
          paint={{
            'line-color': [
              'match',
              ['get', 'kind'],
              'blue',
              '#4a8fd4',
              'red',
              '#cc4444',
              theme === 'light' ? '#555' : '#ccc',
            ] as never,
            'line-width': ['match', ['get', 'kind'], 'mid', 2, 1.5] as never,
            'line-opacity': 0.75,
            'line-dasharray': [2, 2],
          }}
        />
      </Source>

      {/* ── Known enemy air-defence rings (this side's intel only) ── */}
      <Source id="threats" type="geojson" data={threatGeo}>
        <Layer
          id="threats-fill"
          type="fill"
          paint={{ 'fill-color': '#f04747', 'fill-opacity': 0.08 }}
        />
        <Layer
          id="threats-line"
          type="line"
          paint={{
            'line-color': '#f04747',
            'line-width': 1.4,
            'line-dasharray': [3, 2],
            // Faded intel draws fainter — the map should look as uncertain as
            // the picture actually is.
            'line-opacity': ['max', 0.25, ['get', 'confidence']] as never,
          }}
        />
      </Source>

      {/* ── Objectives ──────────────────────────────────────────── */}
      {positioned.map((o) => {
        const r = o.primary ? 9 : 6
        const hot = hotspotByName[o.name]
        // An enemy capture timer already running is the loudest case;
        // `threatened` on its own only means hostiles in the area. Only the
        // first gets the fast red burst, so the alarm keeps its meaning.
        const beingTaken = !!hot?.capture_progress && hot.capture_progress[0] !== report.side
        const underAttack = o.threatened || beingTaken
        return (
          <Marker key={`obj-${o.name}`} latitude={o.lat} longitude={o.lon}>
            <div style={{ position: 'relative', width: r * 2, height: r * 2 }}>
            {underAttack && (
              <span
                className={'vs-underattack' + (beingTaken ? ' vs-underattack-urgent' : '')}
                style={{
                  // Sit the burst off the objective's shoulder rather than on
                  // top of it -- the dot's own fill still has to read.
                  position: 'absolute', left: '50%', bottom: '100%',
                  transform: 'translate(-40%, 30%)',
                  color: beingTaken ? '#f04747' : '#f0a030',
                  display: 'inline-flex', pointerEvents: 'none',
                  filter: 'drop-shadow(0 0 2px rgba(0,0,0,0.85))',
                }}
              >
                <Explosion size={beingTaken ? 15 : 12} />
              </span>
            )}
            <div
              title={`${o.name} — ${o.kind}, ${o.owner}\nhealth ${o.health}% logi ${o.logi}%${
                o.supply != null ? ` supply ${o.supply}%` : ''
              }${o.captureable ? '\nCAPTURABLE NOW' : ''}${beingTaken ? '\nUNDER ATTACK - enemy capture in progress' : o.threatened ? '\nUNDER ATTACK' : ''}`}
              style={{
                width: r * 2,
                height: r * 2,
                borderRadius: '50%',
                background: OWNER_COLOR(o.owner),
                // A knocked-down objective reads as hollow, so "nearly lost"
                // is visible at a glance without clicking anything.
                opacity: 0.35 + (o.health / 100) * 0.65,
                border: o.captureable
                  ? '2px solid #fff'
                  : o.unlimited_aircraft
                    ? `2px solid ${GOLD}`
                    : o.threatened
                      ? '2px solid #f0a030'
                      : `1px solid ${theme === 'light' ? '#0006' : '#fff6'}`,
                boxShadow: o.priority ? '0 0 0 3px #facc1566' : undefined,
                cursor: 'default',
              }}
            />
            <span
              style={{
                position: 'absolute', left: '50%', top: '100%',
                transform: 'translate(-50%, 2px)',
              }}
            >
              <StatusHexes o={o} />
            </span>
            </div>
          </Marker>
        )
      })}

      {/* ── Logistics hubs ──────────────────────────────────────── */}
      {report.logistics.hubs
        .filter((h) => h.lat !== 0 || h.lon !== 0)
        .map((h) => (
          <Marker key={`hub-${h.objective}`} latitude={h.lat} longitude={h.lon}>
            <div
              title={`HUB ${h.objective} — supply ${h.supply}% fuel ${h.fuel}%, feeding ${h.feeding}`}
              style={{
                width: 0,
                height: 0,
                borderLeft: '6px solid transparent',
                borderRight: '6px solid transparent',
                borderBottom: `10px solid ${h.threatened ? '#f04747' : '#facc15'}`,
                transform: 'translateY(-4px)',
              }}
            />
          </Marker>
        ))}

      {/* ── Tasking pins, numbered to match the list ────────────── */}
      {report.tasking
        .filter((t) => t.lat !== 0 || t.lon !== 0)
        .map((t) => {
          const on = selectedTaskId === t.id
          return (
            <Marker
              key={`task-${t.id}`}
              latitude={t.lat}
              longitude={t.lon}
              onClick={(e) => {
                e.originalEvent.stopPropagation()
                onSelectTask(on ? null : t.id)
              }}
            >
              <div
                title={`${t.title} — ${t.detail}`}
                style={{
                  minWidth: 18,
                  height: 18,
                  padding: '0 4px',
                  borderRadius: 4,
                  background: URGENCY_COLOR(t.urgency),
                  color: '#0b0f08',
                  fontFamily: 'var(--font-mono)',
                  fontSize: '0.62rem',
                  fontWeight: 800,
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  border: on ? '2px solid #fff' : '1px solid #0008',
                  transform: on ? 'scale(1.35)' : undefined,
                  cursor: 'pointer',
                  transition: 'transform 120ms',
                }}
              >
                {taskIdx[t.id]}
              </div>
            </Marker>
          )
        })}

      {/* ── Nearest hostile air track ───────────────────────────── */}
      {report.air.nearest && (
        <Marker latitude={report.air.nearest.lat} longitude={report.air.nearest.lon}>
          <div
            title={`${report.air.nearest.class} — ${report.air.nearest.alt_ft} ft, ${report.air.nearest.speed_kts} kt, ${report.air.nearest.range_nm.toFixed(0)}nm off ${report.air.nearest.near}`}
            style={{
              width: 12,
              height: 12,
              transform: `rotate(${report.air.nearest.heading}deg)`,
              clipPath: 'polygon(50% 0%, 100% 100%, 50% 78%, 0% 100%)',
              background: '#f04747',
            }}
          />
        </Marker>
      )}
    </Map>
  )
}
