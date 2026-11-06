import { useMemo } from 'react'
import Map, { Layer, Marker, Source, type MapLayerMouseEvent } from 'react-map-gl/maplibre'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '@shared/web/theme'
import { mapStyleFor } from '../lib/mapStyle'
import type { SituationReport } from '../api'
import type { PlanWaypoint } from './types'

const OWNER_COLOR = (owner: string) =>
  owner === 'Blue' ? '#40a6ff' : owner === 'Red' ? '#b84dff' : '#ffffff'
const ROUTE = '#facc15'
const TARGET = '#f04747'

function ringAround(lat: number, lon: number, radiusM: number, n = 48): number[][] {
  const latPerM = 1 / 111_320
  const lonPerM = 1 / (111_320 * Math.cos((lat * Math.PI) / 180))
  const pts: number[][] = []
  for (let i = 0; i <= n; i++) {
    const a = (i / n) * Math.PI * 2
    pts.push([lon + Math.cos(a) * radiusM * lonPerM, lat + Math.sin(a) * radiusM * latPerM])
  }
  return pts
}

interface Props {
  waypoints: PlanWaypoint[]
  selected: number | null
  /** Situation picture for context: objectives to snap to and known threats. */
  report?: SituationReport
  readOnly: boolean
  onAdd: (lat: number, lon: number, name?: string) => void
  onMove: (i: number, lat: number, lon: number) => void
  onSelect: (i: number | null) => void
}

/**
 * The route over the coalition's own picture. Click the map to add a
 * waypoint, click an objective to add one on it, drag a waypoint to move it.
 * The threat rings are this side's intel from the situation report, the same
 * rings the Situation tab draws.
 */
export default function PlannerMap({ waypoints, selected, report, readOnly, onAdd, onMove, onSelect }: Props) {
  const { theme } = useTheme()
  const mapStyle = useMemo(() => mapStyleFor(theme), [theme])

  const objectives = useMemo(
    () => (report?.map ?? []).filter(o => o.lat !== 0 || o.lon !== 0),
    [report],
  )

  // Centre on the route if there is one, else on the campaign. Only read on
  // mount (and theme change), so editing never yanks the view about.
  const initialViewState = useMemo(() => {
    const pts: [number, number][] = waypoints.length
      ? waypoints.map(w => [w.lat, w.lon])
      : objectives.map(o => [o.lat, o.lon])
    if (!pts.length) return { latitude: 35, longitude: 38, zoom: 4 }
    const lat = pts.reduce((s, p) => s + p[0], 0) / pts.length
    const lon = pts.reduce((s, p) => s + p[1], 0) / pts.length
    return { latitude: lat, longitude: lon, zoom: waypoints.length ? 7 : 6.2 }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme, objectives.length > 0])

  const routeGeo = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: waypoints.length < 2 ? [] : [{
      type: 'Feature' as const,
      properties: {},
      geometry: { type: 'LineString' as const, coordinates: waypoints.map(w => [w.lon, w.lat]) },
    }],
  }), [waypoints])

  const threatGeo = useMemo(() => ({
    type: 'FeatureCollection' as const,
    features: (report?.threats ?? [])
      .filter(t => (t.radius_m ?? 0) > 0)
      .map(t => ({
        type: 'Feature' as const,
        properties: { confidence: t.confidence },
        geometry: { type: 'Polygon' as const, coordinates: [ringAround(t.lat, t.lon, t.radius_m!)] },
      })),
  }), [report])

  const click = (e: MapLayerMouseEvent) => {
    if (readOnly) { onSelect(null); return }
    onAdd(e.lngLat.lat, e.lngLat.lng)
  }

  return (
    <Map
      key={theme}
      mapStyle={mapStyle}
      style={{ width: '100%', height: '100%' }}
      initialViewState={initialViewState}
      dragRotate={false}
      attributionControl={false}
      cursor={readOnly ? 'grab' : 'crosshair'}
      onClick={click}
    >
      <Source id="plan-threats" type="geojson" data={threatGeo}>
        <Layer id="plan-threats-fill" type="fill" paint={{ 'fill-color': TARGET, 'fill-opacity': 0.08 }} />
        <Layer
          id="plan-threats-line"
          type="line"
          paint={{
            'line-color': TARGET, 'line-width': 1.4, 'line-dasharray': [3, 2],
            'line-opacity': ['max', 0.25, ['get', 'confidence']] as never,
          }}
        />
      </Source>

      {objectives.map(o => (
        <Marker
          key={`obj-${o.name}`}
          latitude={o.lat}
          longitude={o.lon}
          onClick={e => {
            e.originalEvent.stopPropagation()
            if (!readOnly) onAdd(o.lat, o.lon, o.name)
          }}
        >
          <div
            title={`${o.name} (${o.kind}, ${o.owner})${readOnly ? '' : '\nclick to add a waypoint here'}`}
            style={{
              width: o.primary ? 14 : 10, height: o.primary ? 14 : 10, borderRadius: '50%',
              background: OWNER_COLOR(o.owner), opacity: 0.75,
              border: `1px solid ${theme === 'light' ? '#0006' : '#fff6'}`,
              cursor: readOnly ? 'default' : 'copy',
            }}
          />
        </Marker>
      ))}

      <Source id="plan-route" type="geojson" data={routeGeo}>
        <Layer id="plan-route-casing" type="line" paint={{ 'line-color': '#000', 'line-width': 4, 'line-opacity': 0.5 }} />
        <Layer id="plan-route-line" type="line" paint={{ 'line-color': ROUTE, 'line-width': 2 }} />
      </Source>

      {waypoints.map((w, i) => (
        <Marker
          key={`wp-${i}`}
          latitude={w.lat}
          longitude={w.lon}
          draggable={!readOnly}
          onDragEnd={e => onMove(i, e.lngLat.lat, e.lngLat.lng)}
          onClick={e => { e.originalEvent.stopPropagation(); onSelect(i) }}
        >
          <div
            title={`${i + 1} ${w.name}${readOnly ? '' : '\ndrag to move'}`}
            style={{
              minWidth: 20, height: 20, padding: '0 4px', borderRadius: w.target ? 2 : 10,
              display: 'flex', alignItems: 'center', justifyContent: 'center',
              fontFamily: 'var(--font-mono)', fontSize: 11, fontWeight: 800,
              background: selected === i ? '#fff' : w.target ? TARGET : ROUTE,
              color: '#0b0f08', border: '2px solid #0b0f08',
              cursor: readOnly ? 'pointer' : 'move',
              boxShadow: selected === i ? `0 0 0 3px ${ROUTE}` : undefined,
            }}
          >
            {i + 1}
          </div>
        </Marker>
      ))}
    </Map>
  )
}
