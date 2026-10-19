/**
 * The maps' base layer. The grey canvas the site started with all but loses
 * Guam and the Marianas in the dark theme, so the default is satellite
 * imagery, with Esri's ocean chart (land, reefs and depths) and the old grey
 * canvas to pick from. All Esri, all served with open CORS, so the
 * kneeboard can inline the same tiles into its PNG.
 */
import { usePref } from './usePref'

export type Basemap = 'satellite' | 'ocean' | 'grey'

export const BASEMAPS: { v: Basemap; label: string; title: string }[] = [
  { v: 'satellite', label: 'Satellite', title: 'Satellite imagery: the islands, runways and reefs as they look' },
  { v: 'ocean', label: 'Ocean', title: 'Ocean chart: coastlines, reefs and sea depths' },
  { v: 'grey', label: 'Grey', title: 'Plain grey canvas: the least behind the overlays' },
]

const ESRI = 'https://server.arcgisonline.com/ArcGIS/rest/services'

/** Tile URL roots ({z}/{y}/{x} follows), the base and its place names. */
export const BASE_TILES: Record<Basemap, { light: string; dark: string; names?: string; maxzoom: number; attribution: string }> = {
  satellite: {
    light: `${ESRI}/World_Imagery/MapServer/tile`,
    dark: `${ESRI}/World_Imagery/MapServer/tile`,
    names: `${ESRI}/Reference/World_Boundaries_and_Places/MapServer/tile`,
    maxzoom: 18,
    attribution: 'Esri, Maxar, Earthstar Geographics',
  },
  ocean: {
    light: `${ESRI}/Ocean/World_Ocean_Base/MapServer/tile`,
    dark: `${ESRI}/Ocean/World_Ocean_Base/MapServer/tile`,
    names: `${ESRI}/Ocean/World_Ocean_Reference/MapServer/tile`,
    // past 13 (and past 10 well inland) Esri serves "map data not yet
    // available": the map stretches the last real tiles instead
    maxzoom: 13,
    attribution: 'Esri, GEBCO, NOAA, National Geographic, Garmin, HERE',
  },
  grey: {
    light: `${ESRI}/Canvas/World_Light_Gray_Base/MapServer/tile`,
    dark: `${ESRI}/Canvas/World_Dark_Gray_Base/MapServer/tile`,
    maxzoom: 16,
    attribution: 'Esri, HERE, Garmin, © OpenStreetMap contributors',
  },
}

export const isBasemap = (v: unknown): v is Basemap => v === 'satellite' || v === 'ocean' || v === 'grey'

/** The base the pilot picked, remembered per browser. */
export function useBasemap() {
  return usePref<Basemap>('range.map.base', 'satellite', isBasemap)
}
