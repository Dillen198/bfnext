// The WebGL layers under the markers: frontline, the tracks a formation has
// left, the road it is about to take (the flowing dash is animated by the
// engine), and every vehicle. The vehicle source's data is pushed by the
// engine, never by React, so it can ease positions ten times a second.
import { memo, useMemo, type ReactElement } from 'react'
import { Layer, Source } from 'react-map-gl/maplibre'
import type { Feature, FeatureCollection } from 'geojson'
import type { Frontlines, GroundFormation } from '../../api'
import { EMPTY_FC, PATH_FLOW_LAYER, VEH_SOURCE } from './engine'
import { ATTACK, NEAR_ZOOM, PENCIL, SIDE_BRIGHT, SIDE_COLOR, WITHDRAW, type Side } from './theme'

const line = (pts: [number, number][]) => pts.map(([lat, lon]) => [lon, lat])

function orderColor(f: GroundFormation, side: Side): string {
  return f.order === 'attack' ? ATTACK : f.order === 'withdraw' ? WITHDRAW : SIDE_BRIGHT[side]
}

interface Props {
  formations: GroundFormation[]
  fronts: Frontlines
  selected: Set<number>
  side: Side
  territory: boolean
}

function BattlefieldLayersImpl({ formations, fronts, selected, side, territory }: Props): ReactElement {
  const frontGeo: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: (['blue', 'red', 'mid'] as const).flatMap((k) =>
      fronts[k].filter((l) => l.length > 1).map((l): Feature => ({
        type: 'Feature',
        properties: { k },
        geometry: { type: 'LineString', coordinates: line(l) },
      })),
    ),
  }), [fronts])

  const trailGeo: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: formations
      .filter((f) => f.trail.length > 1)
      .map((f): Feature => ({
        type: 'Feature',
        properties: { id: f.id },
        // Oldest first, then on to where the column is now.
        geometry: { type: 'LineString', coordinates: line([...f.trail, f.units[f.units.length - 1]?.pos ?? f.pos]) },
      })),
  }), [formations])

  const pathGeo: FeatureCollection = useMemo(() => ({
    type: 'FeatureCollection',
    features: formations
      .filter((f) => f.path.length > 1 && f.order !== 'hold')
      .map((f): Feature => ({
        type: 'Feature',
        properties: { sel: selected.has(f.id) ? 1 : 0, col: orderColor(f, side) },
        geometry: { type: 'LineString', coordinates: line(f.path) },
      })),
  }), [formations, selected, side])

  const own = SIDE_BRIGHT[side]
  const trailGradient = useMemo(
    () => ['interpolate', ['linear'], ['line-progress'], 0, hexA(own, 0), 0.7, hexA(own, 0.35), 1, hexA(own, 0.6)],
    [own],
  )

  return (
    <>
      <Source id="gw-front" type="geojson" data={frontGeo}>
        <Layer
          id="gw-front-glow"
          type="line"
          layout={{ visibility: territory ? 'visible' : 'none' }}
          paint={{
            'line-width': ['match', ['get', 'k'], 'mid', 7, 4],
            'line-color': ['match', ['get', 'k'], 'blue', SIDE_COLOR.Blue, 'red', SIDE_COLOR.Red, '#e6e1cf'],
            'line-opacity': 0.12,
            'line-blur': 3,
          }}
        />
        <Layer
          id="gw-front"
          type="line"
          layout={{ visibility: territory ? 'visible' : 'none' }}
          paint={{
            'line-width': ['match', ['get', 'k'], 'mid', 1.8, 1.1],
            'line-color': ['match', ['get', 'k'], 'blue', SIDE_COLOR.Blue, 'red', SIDE_COLOR.Red, '#e6e1cf'],
            'line-opacity': 0.6,
            'line-dasharray': [3, 2],
          }}
        />
      </Source>

      <Source id="gw-trails" type="geojson" data={trailGeo} lineMetrics>
        {[-1, 1].map((s) => (
          <Layer
            key={s}
            id={`gw-trail-${s < 0 ? 'l' : 'r'}`}
            type="line"
            layout={{ 'line-cap': 'round', 'line-join': 'round' }}
            paint={{
              'line-width': ['interpolate', ['linear'], ['zoom'], 8, 0.8, 14, 1.6],
              'line-offset': ['interpolate', ['linear'], ['zoom'], 8, s * 0.8, 14, s * 2.6],
              // eslint-disable-next-line @typescript-eslint/no-explicit-any
              'line-gradient': trailGradient as any,
            }}
          />
        ))}
      </Source>

      <Source id="gw-paths" type="geojson" data={pathGeo}>
        <Layer
          id="gw-path-base"
          type="line"
          layout={{ 'line-cap': 'round', 'line-join': 'round' }}
          paint={{
            'line-color': ['get', 'col'],
            'line-width': ['match', ['get', 'sel'], 1, 5, 3],
            'line-opacity': ['match', ['get', 'sel'], 1, 0.28, 0.14],
          }}
        />
        <Layer
          id={PATH_FLOW_LAYER}
          type="line"
          layout={{ 'line-join': 'round' }}
          paint={{
            'line-color': ['match', ['get', 'sel'], 1, PENCIL, ['get', 'col']],
            'line-width': ['match', ['get', 'sel'], 1, 2.6, 1.6],
            'line-opacity': ['match', ['get', 'sel'], 1, 0.95, 0.7],
            'line-dasharray': [0, 4, 3],
          }}
        />
      </Source>

      <Source id={VEH_SOURCE} type="geojson" data={EMPTY_FC}>
        <Layer
          id="gw-veh-sel"
          type="circle"
          minzoom={NEAR_ZOOM}
          filter={['==', ['get', 'sel'], 1]}
          paint={{
            'circle-radius': ['interpolate', ['exponential', 1.6], ['zoom'], NEAR_ZOOM, 3, 12, 4, 14, 6.5, 16, 13, 18, 24],
            'circle-color': 'rgba(255,210,63,0.08)',
            'circle-stroke-color': PENCIL,
            'circle-stroke-width': 1.1,
            'circle-stroke-opacity': 0.85,
            'circle-pitch-alignment': 'map',
          }}
        />
        <Layer
          id="gw-veh"
          type="symbol"
          minzoom={NEAR_ZOOM}
          layout={{
            'icon-image': ['get', 'icon'],
            'icon-rotate': ['get', 'rot'],
            'icon-rotation-alignment': 'map',
            'icon-allow-overlap': true,
            'icon-ignore-placement': true,
            // Exaggerated, but small enough that a column on a road still reads as
            // separate vehicles from z14 in.
            'icon-size': ['interpolate', ['exponential', 1.6], ['zoom'], NEAR_ZOOM, 0.26, 12, 0.32, 14, 0.46, 16, 0.9, 18, 1.6],
          }}
          paint={{ 'icon-opacity': ['get', 'op'] }}
        />
      </Source>
    </>
  )
}

function hexA(hex: string, a: number): string {
  const n = parseInt(hex.slice(1), 16)
  return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${a})`
}

export const BattlefieldLayers = memo(BattlefieldLayersImpl)
