import { geoToPpi } from './geo'
import type { GciTerrainHorizon } from './types'

/** Draw terrain-shadow wedges for each EWR horizon (blocked sectors beyond LOS). */
export function drawTerrainShadows(
  ctx: CanvasRenderingContext2D,
  horizons: GciTerrainHorizon[],
  anchorLat: number,
  anchorLon: number,
  cx: number,
  cy: number,
  pxPerNm: number,
  scopeRangeNm: number,
) {
  for (const h of horizons) {
    const origin = geoToPpi(h.lat, h.lon, anchorLat, anchorLon, cx, cy, pxPerNm)
    const capNm = Math.min(h.range_nm, scopeRangeNm)
    const step = h.brg_step || 5

    for (let i = 0; i < h.max_nm.length; i++) {
      const brg0 = i * step
      const brg1 = brg0 + step
      const visNm = h.max_nm[i] ?? 0
      if (visNm >= capNm) continue

      const r0 = visNm * pxPerNm
      const r1 = capNm * pxPerNm
      const a0 = ((brg0 - 90) * Math.PI) / 180
      const a1 = ((brg1 - 90) * Math.PI) / 180

      ctx.beginPath()
      ctx.moveTo(origin.x, origin.y)
      ctx.arc(origin.x, origin.y, r1, a0, a1)
      ctx.arc(origin.x, origin.y, r0, a1, a0, true)
      ctx.closePath()
      ctx.fillStyle = 'rgba(12, 8, 6, 0.55)'
      ctx.fill()
      ctx.strokeStyle = 'rgba(80, 60, 40, 0.35)'
      ctx.lineWidth = 0.5
      ctx.stroke()

      // Radar return reflection off the terrain face
      ctx.beginPath()
      ctx.arc(origin.x, origin.y, r0, a0, a1)
      ctx.strokeStyle = 'rgba(180, 220, 100, 0.45)'
      ctx.lineWidth = 2.0
      ctx.stroke()
    }
  }
}
