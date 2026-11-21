// The Leaflet stand-in's React contexts: the map a component is inside,
// and the source a layer reads (see ./map).
import { createContext, useContext } from 'react'
import type { ShimMap, ShimSource } from './shim'

export const MapCtx = createContext<ShimMap | null>(null)
export const SourceCtx = createContext<ShimSource | null>(null)

/** The Leaflet stand-in for the map these components sit in, if any. */
export function useShimMap(): ShimMap | null {
  return useContext(MapCtx)
}

/** Is this map the Leaflet stand-in? (Typed loosely: it is asked of MapLibre maps.) */
export function isShimMap(m: unknown): boolean {
  return !!(m as { isLeafletGL?: boolean } | null)?.isLeafletGL
}
