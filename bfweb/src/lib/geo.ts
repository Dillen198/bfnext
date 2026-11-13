// Small geo helpers for the recon intel map. Equirectangular local-metre
// approximation — fine at DCS theatre scale and photo footprint sizes.

const M_PER_DEG_LAT = 111_320

export type LatLon = [number, number]

/** Format a lat/lon pair as `25.2369°N 55.4033°E`. */
export function fmtLatLon(lat: number, lon: number): string {
  return (
    `${Math.abs(lat).toFixed(4)}°${lat >= 0 ? 'N' : 'S'} ` +
    `${Math.abs(lon).toFixed(4)}°${lon >= 0 ? 'E' : 'W'}`
  )
}

/** Offset a point by local metres, where `forwardM` points along `headingDeg`
 *  (true) and `rightM` is 90° clockwise of it. */
export function offsetLatLon(
  lat: number,
  lon: number,
  headingDeg: number,
  rightM: number,
  forwardM: number,
): LatLon {
  const h = (headingDeg * Math.PI) / 180
  const east = rightM * Math.cos(h) + forwardM * Math.sin(h)
  const north = -rightM * Math.sin(h) + forwardM * Math.cos(h)
  const mPerDegLon = M_PER_DEG_LAT * Math.cos((lat * Math.PI) / 180)
  return [lat + north / M_PER_DEG_LAT, lon + east / mPerDegLon]
}

// TARPS KS-87 camera assumptions, matching the reference tarps-intel-map:
// 150 mm focal length, 100 mm horizontal frame width.
const FOCAL_MM = 150
const FRAME_MM = 100
const FT_TO_M = 0.3048

/** Approximate rectangular ground footprint of a nadir photo taken at
 *  `altFt` above ground, `headingDeg` true, with image aspect `w/h`.
 *  Returns 4 corners (TL, TR, BR, BL) in the photo's own frame. */
export function groundFootprint(
  lat: number,
  lon: number,
  altFt: number,
  headingDeg: number,
  aspect: number,
): LatLon[] {
  const altM = Math.max(1, altFt * FT_TO_M)
  const widthM = (altM * FRAME_MM) / FOCAL_MM
  const heightM = widthM / (aspect || 1.5)
  const hw = widthM / 2
  const hh = heightM / 2
  return [
    offsetLatLon(lat, lon, headingDeg, -hw, hh),
    offsetLatLon(lat, lon, headingDeg, hw, hh),
    offsetLatLon(lat, lon, headingDeg, hw, -hh),
    offsetLatLon(lat, lon, headingDeg, -hw, -hh),
  ]
}

// ── Tile layers (shared with the tactical map) ─────────────────────────
export const INTEL_TILE_LAYERS = {
  satellite: {
    label: 'SAT',
    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
    attr: 'Esri',
  },
  topo: {
    label: 'TOPO',
    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
    attr: 'Esri',
  },
  tactical: {
    label: 'TACMAP',
    url: 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}',
    attr: 'Esri',
  },
} as const
export type IntelTileKey = keyof typeof INTEL_TILE_LAYERS

/** A "nice" grid step (deg) so that a span shows ~`target` lines. */
export function niceGridStep(spanDeg: number, target = 8): number {
  const raw = spanDeg / target
  const pow = Math.pow(10, Math.floor(Math.log10(raw)))
  const n = raw / pow
  const step = n >= 5 ? 5 : n >= 2 ? 2 : n >= 1 ? 1 : 0.5
  return step * pow
}

/** Format a grid-line label, e.g. 32.5 → "32°30'E". */
export function fmtGridLabel(v: number, axis: 'lat' | 'lon'): string {
  const hemi = axis === 'lat' ? (v >= 0 ? 'N' : 'S') : (v >= 0 ? 'E' : 'W')
  const a = Math.abs(v)
  const d = Math.floor(a)
  const m = Math.round((a - d) * 60)
  return m ? `${d}°${String(m).padStart(2, '0')}'${hemi}` : `${d}°${hemi}`
}
