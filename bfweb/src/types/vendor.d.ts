// Minimal ambient types for two untyped deps used by the TACMAP, matching
// how peace-eye (pbzweihander/peace-eye) uses them.

declare module 'mgrs' {
  /** [lon, lat] → MGRS grid string. */
  export function forward(ll: [number, number], accuracy?: number): string
  export function toPoint(mgrs: string): [number, number]
}

declare module '@turf/circle' {
  import type { Feature, Polygon } from 'geojson'
  interface Options {
    steps?: number
    units?: 'kilometers' | 'miles' | 'degrees' | 'radians'
    properties?: Record<string, unknown>
  }
  export default function circle(
    center: [number, number] | Feature,
    radius: number,
    options?: Options,
  ): Feature<Polygon>
}

declare module 'geomagnetism' {
  interface GeomagPoint {
    /** Magnetic declination (variation) in degrees, east positive. */
    decl: number
    incl: number
    ti: number
  }
  interface GeomagModel {
    point(latlon: [number, number]): GeomagPoint
  }
  /** WMM model for `date` (defaults to now; throws outside its validity window). */
  export function model(date?: Date): GeomagModel
}
