/**
 * The base map. Same approach as bfweb's scope page (src/scope/ScopePage.tsx):
 * an inline raster style off Esri's canvas tiles, because a hosted vector
 * style was blocked by the production CSP and rendered black. Follows the
 * site theme.
 */
import { useMemo, type ReactNode } from 'react'
import Map, { AttributionControl, NavigationControl, ScaleControl, type MapProps } from 'react-map-gl/maplibre'
import type { StyleSpecification } from 'maplibre-gl'
import 'maplibre-gl/dist/maplibre-gl.css'
import { useTheme } from '../context/ThemeContext'

function mapStyleFor(theme: 'dark' | 'light'): StyleSpecification {
  return {
    version: 8,
    sources: {
      esri: {
        type: 'raster',
        tiles: [
          theme === 'light'
            ? 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
            : 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}',
        ],
        tileSize: 256,
        maxzoom: 16,
        attribution: 'Esri, HERE, Garmin, © OpenStreetMap contributors',
      },
    },
    layers: [
      { id: 'bg', type: 'background', paint: { 'background-color': theme === 'light' ? '#dfe3e7' : '#0a0e12' } },
      { id: 'esri', type: 'raster', source: 'esri', paint: { 'raster-opacity': theme === 'light' ? 0.95 : 0.8 } },
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
} & Omit<MapProps, 'mapStyle' | 'initialViewState' | 'style'>) {
  const { theme } = useTheme()
  const style = useMemo(() => mapStyleFor(theme), [theme])
  const initialViewState = bounds
    ? { bounds, fitBoundsOptions: { padding: 40, maxZoom: 14 } }
    : { latitude: center?.lat ?? 42, longitude: center?.lon ?? 41.8, zoom }
  return (
    <div className={`map-shell ${className ?? ''}`} style={{ height }}>
      <Map
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
