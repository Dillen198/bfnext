// The command map's situation overlays, drawn under the units:
//
// - territory: each base's zone of control (the ground nearer to it than to
//   any other base, out to TERRITORY_KM), washed in its owner's colour, so
//   the front reads as a boundary between two shaded areas, not a line;
// - our supply network: hub -> base, solid when it runs, red and broken
//   where it is cut;
// - our air-defence reach and radar coverage;
// - the enemy air defences we know of, with how sure we are where they are.
//
// All of it is our own side's picture: the engine and /ws/tacmap only ever
// send a side what it is entitled to know.
import { memo, useMemo, type ReactElement } from 'react'
import { Layer, Source } from 'react-map-gl/maplibre'
import circle from '@turf/circle'
import type { Feature, FeatureCollection } from 'geojson'
import type { DefenceRing, GroundObjective, LatLon, SupplyLine, TacPicture } from '../../api'
import { territoryCells } from './territory'
import { SIDE_BRIGHT, SIDE_COLOR, other, type Side } from './theme'

const ownerColor: unknown = ['match', ['get', 'owner'], 'Blue', SIDE_COLOR.Blue, 'Red', SIDE_COLOR.Red, SIDE_COLOR.Neutral]

function TerritoryImpl({ objectives, visible }: { objectives: GroundObjective[]; visible: boolean }): ReactElement {
  // Only ownership and place matter; the picture's other churn mustn't redo it.
  const key = objectives.map((o) => `${o.id}:${o.owner}`).join(',')
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const geo = useMemo(() => territoryCells(objectives), [key])
  const vis = visible ? 'visible' : 'none'
  return (
    <Source id="cm-territory" type="geojson" data={geo}>
      <Layer id="cm-territory-fill" type="fill" layout={{ visibility: vis }}
        paint={{ 'fill-color': ownerColor as never, 'fill-opacity': 0.1 }} />
      <Layer id="cm-territory-edge" type="line" layout={{ visibility: vis }}
        paint={{ 'line-color': ownerColor as never, 'line-opacity': 0.22, 'line-width': 1 }} />
    </Source>
  )
}
export const Territory = memo(TerritoryImpl)

function SupplyImpl({ lines, side, visible }: { lines: SupplyLine[]; side: Side; visible: boolean }): ReactElement {
  const geo = useMemo<FeatureCollection>(() => ({
    type: 'FeatureCollection',
    features: lines.map((l) => ({
      type: 'Feature',
      properties: { cut: l.cut ? 1 : 0, name: `${l.from_name} → ${l.to_name}` },
      geometry: { type: 'LineString', coordinates: [[l.from[1], l.from[0]], [l.to[1], l.to[0]]] },
    })),
  }), [lines])
  const vis = visible ? 'visible' : 'none'
  return (
    <Source id="cm-supply" type="geojson" data={geo}>
      <Layer id="cm-supply-ok" type="line" filter={['==', ['get', 'cut'], 0]} layout={{ visibility: vis, 'line-cap': 'round' }}
        paint={{ 'line-color': '#e8c547', 'line-opacity': 0.75, 'line-width': 2, 'line-dasharray': [5, 3] }} />
      <Layer id="cm-supply-cut" type="line" filter={['==', ['get', 'cut'], 1]} layout={{ visibility: vis }}
        paint={{ 'line-color': '#ff5b45', 'line-opacity': 0.75, 'line-width': 1.8, 'line-dasharray': [1.5, 2.5] }} />
    </Source>
  )
}
export const SupplyNetwork = memo(SupplyImpl)

function ring(pos: LatLon, m: number) {
  return circle([pos[1], pos[0]], Math.max(m, 50) / 1000, { steps: 56, units: 'kilometers' })
}

function CoverageImpl({ defences, tac, side, visible }: {
  defences: DefenceRing[]
  tac: TacPicture | null
  side: Side
  visible: boolean
}): ReactElement {
  const geo = useMemo<FeatureCollection>(() => {
    const features: Feature[] = []
    for (const d of defences) {
      const f = ring(d.pos, d.range_m)
      f.properties = { k: d.kind, live: d.live ? 1 : 0 }
      features.push(f)
    }
    for (const r of tac?.radar_rings ?? []) {
      if (!r.alive) continue
      const f = ring([r.lat, r.lon], r.range_m)
      f.properties = { k: 'radar', live: 1 }
      features.push(f)
    }
    return { type: 'FeatureCollection', features }
  }, [defences, tac])
  const vis = visible ? 'visible' : 'none'
  const own = SIDE_BRIGHT[side]
  return (
    <Source id="cm-cover" type="geojson" data={geo}>
      <Layer id="cm-cover-radar" type="line" filter={['==', ['get', 'k'], 'radar']} layout={{ visibility: vis }}
        paint={{ 'line-color': own, 'line-opacity': 0.18, 'line-width': 1, 'line-dasharray': [2, 4] }} />
      <Layer id="cm-cover-ad" type="fill" filter={['!=', ['get', 'k'], 'radar']} layout={{ visibility: vis }}
        paint={{ 'fill-color': own, 'fill-opacity': ['case', ['==', ['get', 'live'], 1], 0.07, 0.03] }} />
      <Layer id="cm-cover-ad-edge" type="line" filter={['!=', ['get', 'k'], 'radar']} layout={{ visibility: vis }}
        paint={{ 'line-color': own, 'line-opacity': ['case', ['==', ['get', 'live'], 1], 0.6, 0.3], 'line-width': ['match', ['get', 'k'], 'sam', 1.4, 0.9] }} />
    </Source>
  )
}
export const Coverage = memo(CoverageImpl)

function ThreatsImpl({ tac, side, visible }: { tac: TacPicture | null; side: Side; visible: boolean }): ReactElement {
  const enemy = other(side)
  const geo = useMemo<FeatureCollection>(() => {
    const features: Feature[] = []
    for (const g of tac?.ground ?? []) {
      if (!g.threat_range_m || (g.side != null && g.side.toLowerCase() !== enemy.toLowerCase())) continue
      // Older and less certain contacts fade: where it was, not where it is.
      const sure = Math.max(0.2, Math.min(1, g.confidence)) * (g.age_s > 1800 ? 0.5 : 1)
      const f = ring([g.lat, g.lon], g.threat_range_m)
      f.properties = { k: 'threat', a: sure }
      features.push(f)
      if (g.uncertainty_m > 300) {
        const u = ring([g.lat, g.lon], g.uncertainty_m)
        u.properties = { k: 'where', a: sure }
        features.push(u)
      }
    }
    return { type: 'FeatureCollection', features }
  }, [tac, enemy])
  const vis = visible ? 'visible' : 'none'
  const col = SIDE_BRIGHT[enemy]
  return (
    <Source id="cm-threats" type="geojson" data={geo}>
      <Layer id="cm-threat-fill" type="fill" filter={['==', ['get', 'k'], 'threat']} layout={{ visibility: vis }}
        paint={{ 'fill-color': col, 'fill-opacity': ['*', 0.08, ['get', 'a']] }} />
      <Layer id="cm-threat-edge" type="line" filter={['==', ['get', 'k'], 'threat']} layout={{ visibility: vis }}
        paint={{ 'line-color': col, 'line-opacity': ['*', 0.8, ['get', 'a']], 'line-width': 1.4 }} />
      <Layer id="cm-threat-where" type="line" filter={['==', ['get', 'k'], 'where']} layout={{ visibility: vis }}
        paint={{ 'line-color': col, 'line-opacity': ['*', 0.5, ['get', 'a']], 'line-width': 1, 'line-dasharray': [1, 2] }} />
    </Source>
  )
}
export const Threats = memo(ThreatsImpl)

/** The route a Move is being given, waypoint by waypoint, before it is sent. */
export function RoutePreview({ from, wps, side }: { from: LatLon[]; wps: LatLon[]; side: Side }): ReactElement | null {
  const geo = useMemo<FeatureCollection>(() => ({
    type: 'FeatureCollection',
    features: [
      ...from.map((f) => ({
        type: 'Feature' as const,
        properties: {},
        geometry: { type: 'LineString' as const, coordinates: [f, ...wps].map((p) => [p[1], p[0]]) },
      })),
      ...wps.map((p, i) => ({
        type: 'Feature' as const,
        properties: { n: String(i + 1) },
        geometry: { type: 'Point' as const, coordinates: [p[1], p[0]] },
      })),
    ],
  }), [from, wps])
  if (wps.length === 0) return null
  return (
    <Source id="cm-route" type="geojson" data={geo}>
      <Layer id="cm-route-line" type="line" filter={['==', ['geometry-type'], 'LineString']}
        paint={{ 'line-color': SIDE_BRIGHT[side], 'line-width': 2, 'line-dasharray': [2, 1.5], 'line-opacity': 0.9 }} />
      <Layer id="cm-route-pt" type="circle" filter={['==', ['geometry-type'], 'Point']}
        paint={{ 'circle-radius': 5, 'circle-color': '#0b0d0b', 'circle-stroke-color': SIDE_BRIGHT[side], 'circle-stroke-width': 2 }} />
    </Source>
  )
}
