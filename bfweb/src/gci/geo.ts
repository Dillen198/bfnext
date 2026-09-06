const NM_M = 1852

/** Great-circle distance in metres between two WGS84 points. */
export function distM(lat1: number, lon1: number, lat2: number, lon2: number): number {
  const r = 6371000
  const p1 = (lat1 * Math.PI) / 180
  const p2 = (lat2 * Math.PI) / 180
  const dp = ((lat2 - lat1) * Math.PI) / 180
  const dl = ((lon2 - lon1) * Math.PI) / 180
  const a =
    Math.sin(dp / 2) ** 2 +
    Math.cos(p1) * Math.cos(p2) * Math.sin(dl / 2) ** 2
  return 2 * r * Math.asin(Math.sqrt(a))
}

/** Initial bearing degrees true from (lat1,lon1) to (lat2,lon2). */
export function bearingDeg(lat1: number, lon1: number, lat2: number, lon2: number): number {
  const p1 = (lat1 * Math.PI) / 180
  const p2 = (lat2 * Math.PI) / 180
  const dl = ((lon2 - lon1) * Math.PI) / 180
  const y = Math.sin(dl) * Math.cos(p2)
  const x = Math.cos(p1) * Math.sin(p2) - Math.sin(p1) * Math.cos(p2) * Math.cos(dl)
  return ((Math.atan2(y, x) * 180) / Math.PI + 360) % 360
}

/** Map geo point to PPI canvas coords; scope center = anchor, north up. */
export function geoToPpi(
  lat: number,
  lon: number,
  anchorLat: number,
  anchorLon: number,
  cx: number,
  cy: number,
  pxPerNm: number,
): { x: number; y: number; rngNm: number; brg: number } {
  const rngM = distM(anchorLat, anchorLon, lat, lon)
  const rngNm = rngM / NM_M
  const brg = bearingDeg(anchorLat, anchorLon, lat, lon)
  const rad = (brg * Math.PI) / 180
  const r = rngNm * pxPerNm
  return {
    x: cx + r * Math.sin(rad),
    y: cy - r * Math.cos(rad),
    rngNm,
    brg,
  }
}
