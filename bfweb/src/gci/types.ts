export type GciDesk = 'blue' | 'red' | 'god'

export interface GciTrack {
  id: number
  tn: string
  iff: number
  cls: number
  lat: number
  lon: number
  alt_ft: number
  hdg: number
  spd_kts: number
  brg: number
  rng_nm: number
  age: number
  stale: boolean
  src: number
  conf: number
  contested?: boolean
  jam?: number
}

export interface GciJamZone {
  lat: number
  lon: number
  radius_nm: number
  strength: number
  label?: string
}

export interface GciDonor {
  side: string
  lat: number
  lon: number
  range_m: number
  airborne: boolean
  name?: string
}

export interface GciTerrainHorizon {
  side: string
  lat: number
  lon: number
  range_nm: number
  brg_step: number
  max_nm: number[]
  airborne?: boolean
}

export interface GciBullseye {
  side: number
  lat: number
  lon: number
}

export interface GciAnchor {
  lat: number
  lon: number
  label: string
}

export interface WsGciMsg {
  time: string
  theater?: string
  desk: string
  anchor: GciAnchor
  bull: GciBullseye[]
  tracks: GciTrack[]
  donors: GciDonor[]
  terrain?: GciTerrainHorizon[]
  jam_zones?: GciJamZone[]
}

export function iffLabel(iff: number): 'hostile' | 'friendly' | 'unknown' {
  if (iff === 1) return 'friendly'
  if (iff === 0) return 'hostile'
  return 'unknown'
}

export function srcLabel(src: number): string {
  if (src === 3) return 'GA'
  if (src === 2) return 'A'
  if (src === 1) return 'G'
  return ''
}

export function clsLabel(cls: number): string {
  switch (cls) {
    case 1: return 'FTR'
    case 2: return 'BMB'
    case 3: return 'HEL'
    default: return 'UNK'
  }
}
