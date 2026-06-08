import { bearingDeg, distM } from './geo'
import type { GciAnchor, GciDonor, GciTrack, WsGciMsg } from './types'

const NM_M = 1852

export type GciFocusMode = 'bullseye' | 'donor' | 'track'

export function donorLabel(d: GciDonor, index: number): string {
  const nm = Math.round(d.range_m / 1852)
  if (d.name) {
    return `${d.name} (${nm}NM)`
  }
  const kind = d.airborne ? 'AWACS' : d.range_m > 80_000 ? 'EWR' : 'RADAR'
  return `${kind}${index + 1} (${nm}NM)`
}

/** PPI center and BRG/RNG reference for the current focus mode. */
export function resolveAnchor(
  msg: WsGciMsg | null,
  mode: GciFocusMode,
  donorIndex: number,
  selectedTrack: GciTrack | null,
): GciAnchor | null {
  if (!msg) return null

  if (mode === 'donor') {
    const d = msg.donors[donorIndex]
    if (d) {
      return {
        lat: d.lat,
        lon: d.lon,
        label: donorLabel(d, donorIndex),
      }
    }
  }

  if (mode === 'track' && selectedTrack) {
    return {
      lat: selectedTrack.lat,
      lon: selectedTrack.lon,
      label: selectedTrack.tn,
    }
  }

  return msg.anchor
}

/** Recompute BRG/RNG from the active PPI anchor (client-side). */
export function tracksRelativeToAnchor(tracks: GciTrack[], anchor: GciAnchor): GciTrack[] {
  return tracks.map((t) => {
    const brg = Math.round(bearingDeg(anchor.lat, anchor.lon, t.lat, t.lon))
    const rng_nm = Math.min(999, Math.round(distM(anchor.lat, anchor.lon, t.lat, t.lon) / NM_M))
    return { ...t, brg, rng_nm }
  })
}

/** Suggested PPI range when focusing a ground/air donor. */
export function suggestedRangeNm(d: GciDonor): number {
  const nm = Math.ceil(d.range_m / NM_M)
  const steps = [50, 100, 150, 200] as const
  for (const s of steps) {
    if (nm <= s) return s
  }
  return 200
}
