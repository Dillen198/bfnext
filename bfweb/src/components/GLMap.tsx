// react-map-gl's MapLibre <Map>, <Marker>, <Source> and <Layer> -- or, in a
// browser without WebGL such as DCS World's embedded one (see lib/webgl), the
// same components drawn with Leaflet (lib/leafletgl), so the pages' layers,
// markers and map code all still work there. The map pages import these in
// place of react-map-gl's; Marker, Source and Layer follow whichever map
// they are inside. The Leaflet side is loaded only when it is needed.
import { forwardRef, lazy, Suspense, type ComponentProps, type ReactNode } from 'react'
import MapGL, {
  Layer as MlLayer, Marker as MlMarker, Source as MlSource, type MapRef,
} from 'react-map-gl/maplibre'
import { webglSupport } from '../lib/webgl'
import { useShimMap } from '../lib/leafletgl/context'
import type { LayerSpec } from '../lib/leafletgl/shim'

// Loaded once, the first time a map needs it. Marker / Source / Layer only
// ever render inside that map, by when it is loaded: they use it directly
// rather than as lazy components, because a lazy child suspending would hide
// the map, and React takes a hidden tree's layout effects down with it.
type ShimModule = typeof import('../lib/leafletgl/map')
let shim: ShimModule | null = null
const LeafletGLMap = lazy(() => import('../lib/leafletgl/map').then((m) => {
  shim = m
  return { default: m.LeafletGLMap }
}))

type MapProps = ComponentProps<typeof MapGL>

const GLMap = forwardRef<MapRef, MapProps>(function GLMap(props, ref) {
  if (webglSupport().ok) return <MapGL ref={ref} {...props} />
  // The stand-in has the parts of MapRef the pages call (lib/leafletgl/shim).
  return (
    <Suspense fallback={null}>
      {/* eslint-disable-next-line @typescript-eslint/no-explicit-any */}
      <LeafletGLMap ref={ref as any} {...(props as any)} />
    </Suspense>
  )
})

export default GLMap

export function Marker(props: ComponentProps<typeof MlMarker>) {
  const inShim = useShimMap()
  if (!inShim || !shim) return <MlMarker {...props} />
  const { longitude, latitude, anchor, offset, children } = props
  return (
    <shim.ShimMarker longitude={longitude} latitude={latitude} anchor={anchor}
      offset={offset as [number, number] | { x: number; y: number } | undefined}>
      {children}
    </shim.ShimMarker>
  )
}

export function Source(props: ComponentProps<typeof MlSource>) {
  const inShim = useShimMap()
  if (!inShim || !shim) return <MlSource {...props} />
  const p = props as { id?: string; data?: unknown; children?: ReactNode }
  return <shim.ShimSourceEl id={p.id ?? 'src'} data={p.data}>{p.children}</shim.ShimSourceEl>
}

export function Layer(props: ComponentProps<typeof MlLayer>) {
  const inShim = useShimMap()
  if (!inShim || !shim) return <MlLayer {...props} />
  return <shim.ShimLayerEl {...(props as unknown as LayerSpec)} />
}
