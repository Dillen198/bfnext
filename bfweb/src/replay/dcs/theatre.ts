// What the replay knows about DCS's own maps: which theatre a point is in,
// its DCS mission-editor X/Z (each map is a Transverse Mercator with its own
// central meridian and false origin -- the scope's terrain table), and the
// airfields and runways to draw on the F10-style map.
//
// Runways come from OurAirports (public domain; bfweb/scripts/
// gen-dcs-airfields.py filters them to the theatres). Where the scope's DCS
// airport list has a field within 3 km, its DCS name is used, so the map
// reads like the F10 map.

import { Terrains, type Terrain } from '../../scope/dcs/terrain'
import { getRange, toMercProj } from '../../scope/util'

/** The DCS theatre whose centre is nearest, within 600 km. */
export function theatreAt(lat: number, lon: number): Terrain | null {
  let best: { t: Terrain; d: number } | null = null
  for (const t of Terrains) {
    const d = getRange([lat, lon], t.center)
    if (d < 600 && (!best || d < best.d)) best = { t, d }
  }
  return best?.t ?? null
}

/** DCS mission-editor coordinates: X north, Z east, metres. */
export function dcsXZ(lat: number, lon: number, t: Terrain): { x: number; z: number } {
  const [east, north] = toMercProj([lat, lon], t)
  return { x: north, z: east }
}

/** [le ident, he ident, le lat, le lon, he lat, he lon, length ft, width ft] */
export type Runway = [string, string, number, number, number, number, number, number]

export interface Airfield {
  theatre: string
  name: string
  code: string
  lat: number
  lon: number
  elevFt: number
  runways: Runway[]
}

let loading: Promise<Airfield[]> | null = null

/** Every airfield in every DCS theatre (loaded once, on first use). */
export function airfields(): Promise<Airfield[]> {
  loading ??= import('./airfields.json').then(m => {
    const rows = m.default as unknown as [string, string, string, number, number, number, Runway[]][]
    return rows.map(([theatre, name, code, lat, lon, elevFt, runways]) => {
      // The DCS name, when the scope's airport list has this field.
      let dcsName: string | null = null
      for (const t of Terrains) {
        if (getRange([lat, lon], t.center) > 700) continue
        for (const a of t.airports) {
          if (!a.heli && getRange([lat, lon], a.position) < 3) { dcsName = a.name; break }
        }
        if (dcsName) break
      }
      return { theatre, name: dcsName ?? name, code, lat, lon, elevFt, runways }
    })
  })
  return loading
}
