// The three map looks. All of them are inline raster styles off Esri's tile
// servers: a hosted vector style is blocked by the dashboard's prod CSP, and
// raster images are allowed from any https origin.
//
// SAT is the default because a battlefield should look like ground: imagery,
// pulled down in saturation and brightness so the symbols on it carry the
// colour, with hillshade laid over it for relief and the place names faint on
// top. TOPO is the paper map, muted. TAC is the dashboard's own dark canvas.
import type { StyleSpecification } from 'maplibre-gl'
import { mapStyleFor } from '../../lib/mapStyle'

export type MapLook = 'sat' | 'topo' | 'tac'
export const MAP_LOOKS: { key: MapLook; label: string; hint: string }[] = [
  { key: 'sat', label: 'SAT', hint: 'Satellite imagery with relief' },
  { key: 'topo', label: 'TOPO', hint: 'Topographic map' },
  { key: 'tac', label: 'TAC', hint: 'Plain tactical canvas' },
]

const ESRI = 'https://server.arcgisonline.com/ArcGIS/rest/services'

function raster(path: string, maxzoom: number) {
  return {
    type: 'raster' as const,
    tiles: [`${ESRI}/${path}/MapServer/tile/{z}/{y}/{x}`],
    tileSize: 256,
    maxzoom,
    attribution: 'Esri',
  }
}

export function styleFor(look: MapLook, theme: string): StyleSpecification {
  if (look === 'tac') return mapStyleFor(theme) as StyleSpecification
  if (look === 'topo') {
    return {
      version: 8,
      sources: { topo: raster('World_Topo_Map', 19) },
      layers: [
        { id: 'bg', type: 'background', paint: { 'background-color': '#11140f' } },
        {
          id: 'topo',
          type: 'raster',
          source: 'topo',
          paint: {
            // Dimmed like a paper map under a red torch, so the symbols keep the light.
            'raster-saturation': -0.5,
            'raster-brightness-max': 0.5,
            'raster-contrast': 0.15,
          },
        },
      ],
    }
  }
  return {
    version: 8,
    sources: {
      sat: raster('World_Imagery', 19),
      relief: raster('Elevation/World_Hillshade', 16),
      places: raster('Reference/World_Boundaries_and_Places', 13),
    },
    layers: [
      { id: 'bg', type: 'background', paint: { 'background-color': '#0b0e0a' } },
      {
        id: 'sat',
        type: 'raster',
        source: 'sat',
        paint: {
          'raster-saturation': -0.45,
          'raster-brightness-max': 0.8,
          'raster-contrast': 0.12,
        },
      },
      {
        id: 'relief',
        type: 'raster',
        source: 'relief',
        paint: {
          'raster-opacity': 0.22,
          'raster-brightness-max': 0.55,
          'raster-contrast': 0.3,
        },
      },
      {
        id: 'places',
        type: 'raster',
        source: 'places',
        // Past this the reference tiles are overscaled into giant blurred type.
        maxzoom: 12.5,
        paint: { 'raster-opacity': 0.42, 'raster-saturation': -1 },
      },
    ],
  }
}
