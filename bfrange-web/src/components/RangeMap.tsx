/**
 * The base map. Same approach as bfweb's scope page (src/scope/ScopePage.tsx):
 * an inline raster style off Esri's canvas tiles, because a hosted vector
 * style was blocked by the production CSP and rendered black. Follows the
 * site theme; the base is the viewer's pick (lib/basemap.ts), satellite
 * unless they chose otherwise.
 */
import { useMemo, type ReactNode, type Ref } from 'react'
import Map, { AttributionControl, NavigationControl, ScaleControl, type MapProps, type MapRef } from 'react-map-gl/maplibre'
import type { StyleSpecification } from 'maplibre-gl'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '@shared/web/theme'
import { BASE_TILES, useBasemap, type Basemap } from '../lib/basemap'

function mapStyleFor(theme: 'dark' | 'light', base: Basemap, withNames: boolean): StyleSpecification {
  const t = BASE_TILES[base]
  const names = withNames ? t.names : undefined
  const dark = theme === 'dark'
  const paint: Record<Basemap, Record<string, number>> = {
    // imagery and the ocean chart are taken down a little in the dark theme
    // so the overlays still stand off them; the islands stay plain to see
    satellite: { 'raster-opacity': dark ? 0.9 : 1, 'raster-brightness-max': dark ? 0.85 : 1 },
    ocean: { 'raster-opacity': 1, 'raster-brightness-max': dark ? 0.62 : 1, 'raster-saturation': dark ? -0.15 : 0 },
    grey: { 'raster-opacity': dark ? 0.8 : 0.95 },
  }
  const bg = base === 'satellite' ? '#0d1a26' : base === 'ocean' ? (dark ? '#22303d' : '#c9dbe8') : dark ? '#0a0e12' : '#dfe3e7'
  return {
    version: 8,
    sources: {
      esri: { type: 'raster', tiles: [`${dark ? t.dark : t.light}/{z}/{y}/{x}`], tileSize: 256, maxzoom: t.maxzoom, attribution: t.attribution },
      ...(names ? { names: { type: 'raster' as const, tiles: [`${names}/{z}/{y}/{x}`], tileSize: 256, maxzoom: 13 } } : {}),
    },
    layers: [
      { id: 'bg', type: 'background', paint: { 'background-color': bg } },
      { id: 'esri', type: 'raster', source: 'esri', paint: paint[base] },
      ...(names ? [{ id: 'names', type: 'raster' as const, source: 'names', paint: { 'raster-opacity': dark ? 0.75 : 0.9 } }] : []),
    ],
  }
}

export function RangeMap({
  children,
  height,
  bounds,
  center,
  zoom = 9,
  className,
  controls = true,
  mapRef,
  imagery = false,
  base,
  ...rest
}: {
  children?: ReactNode
  height: number | string
  /** [[west, south], [east, north]] */
  bounds?: [[number, number], [number, number]] | null
  center?: { lat: number; lon: number }
  zoom?: number
  className?: string
  controls?: boolean
  /** for flying the camera (the control page's focus) */
  mapRef?: Ref<MapRef>
  /** satellite imagery whatever the viewer picked (taxiways, aprons) */
  imagery?: boolean
  /** the base layer; the viewer's remembered pick when left out */
  base?: Basemap
} & Omit<MapProps, 'mapStyle' | 'initialViewState' | 'style'>) {
  const { theme } = useTheme()
  const [picked] = useBasemap()
  const b: Basemap = imagery ? 'satellite' : base ?? picked
  // a forced close-up (a debrief's taxiways) does without the place names
  const style = useMemo(() => mapStyleFor(theme, b, !imagery), [theme, b, imagery])
  const initialViewState = bounds
    ? { bounds, fitBoundsOptions: { padding: 40, maxZoom: 14 } }
    : { latitude: center?.lat ?? 14.3, longitude: center?.lon ?? 145.2, zoom }
  return (
    <div className={`map-shell ${className ?? ''}`} style={{ height }}>
      <Map
        ref={mapRef}
        mapStyle={style}
        initialViewState={initialViewState}
        style={{ width: '100%', height: '100%' }}
        attributionControl={false}
        dragRotate={false}
        touchPitch={false}
        {...rest}
      >
        <AttributionControl position="bottom-right" compact />
        {controls && <NavigationControl position="top-right" showCompass={false} />}
        {controls && <ScaleControl position="bottom-left" unit="nautical" />}
        {children}
      </Map>
    </div>
  )
}
