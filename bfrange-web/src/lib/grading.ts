/**
 * Grading thresholds from `bfprotocols/src/range/grading.rs` and the
 * `ScoringCfg` defaults in `cfg.rs`, for legends and hover text. The engine
 * grades; the site only explains. A server can override these in its config,
 * so legends say "default".
 */
import type {
  BombQuality,
  PrecisionQuality,
  StrafeQuality,
  WeaponClass,
} from '../types'

export const SCORING_DEFAULTS = {
  shack_m: 1.53,
  good_unguided_m: 25,
  good_guided_m: 10,
  good_rocket_m: 30,
  /** DEADEYE, EXCELLENT, GOOD, INEFFECTIVE; below the last is POOR */
  strafe_bands: [90, 75, 50, 25] as const,
}

export function goodRadius(cls: WeaponClass): number {
  switch (cls) {
    case 'guided':
    case 'missile': return SCORING_DEFAULTS.good_guided_m
    case 'rocket': return SCORING_DEFAULTS.good_rocket_m
    default: return SCORING_DEFAULTS.good_unguided_m
  }
}

/** MOOSE RANGE bands; mirrors `grading::bomb_quality`. */
export function bombQuality(cls: WeaponClass, missM: number, good = goodRadius(cls)): BombQuality {
  if (missM <= SCORING_DEFAULTS.shack_m) return 'SHACK'
  if (missM <= good * 0.5) return 'EXCELLENT'
  if (missM <= good) return 'GOOD'
  if (missM <= good * 2) return 'INEFFECTIVE'
  return 'POOR'
}

/** Band radii for a GOOD radius, innermost first (SHACK, EXCELLENT, GOOD, INEFFECTIVE). */
export function bombBands(good: number): { quality: BombQuality; r: number }[] {
  return [
    { quality: 'SHACK', r: SCORING_DEFAULTS.shack_m },
    { quality: 'EXCELLENT', r: good * 0.5 },
    { quality: 'GOOD', r: good },
    { quality: 'INEFFECTIVE', r: good * 2 },
  ]
}

export function strafeQuality(accuracyPct: number, valid = true): StrafeQuality {
  if (!valid) return 'INVALID'
  const [deadeye, excellent, good, ineffective] = SCORING_DEFAULTS.strafe_bands
  if (accuracyPct >= deadeye) return 'DEADEYE'
  if (accuracyPct >= excellent) return 'EXCELLENT'
  if (accuracyPct >= good) return 'GOOD'
  if (accuracyPct >= ineffective) return 'INEFFECTIVE'
  return 'POOR'
}

/** PERFECT inside `perfect`, EXCELLENT 2x, GOOD 4x, FAIR 8x, else POOR. */
export function precisionQuality(perfectM: number, distanceM: number): PrecisionQuality {
  if (distanceM <= perfectM) return 'PERFECT'
  if (distanceM <= perfectM * 2) return 'EXCELLENT'
  if (distanceM <= perfectM * 4) return 'GOOD'
  if (distanceM <= perfectM * 8) return 'FAIR'
  return 'POOR'
}

/** Clock position of an impact relative to the attack heading; mirrors `grading::clock`. */
export function clock(relDeg: number): number {
  const r = ((relDeg % 360) + 360) % 360
  const c = Math.floor((r + 15) / 30) % 12
  return c === 0 ? 12 : c
}

/** Glideslope call bands (degrees, + high): little / normal / lot. */
export const GS_BANDS = { high: [0.4, 0.8, 1.5], low: [-0.3, -0.6, -0.9] } as const
/** Lineup call bands (degrees either side). */
export const LU_BANDS = [0.5, 1.0, 3.0] as const
/** Automatic waveoff limits inside the in-close window. */
export const WAVEOFF_LIMITS = { lue: 3.0, gse_low: -1.2, gse_high: 1.8 } as const

/** The drawn band lines on a trap sheet: ±0.4 / ±0.8 / ±1.5 degrees. */
export const TRAP_SHEET_BANDS = [
  { deg: 0.4, color: 'var(--band-1)' },
  { deg: 0.8, color: 'var(--band-2)' },
  { deg: 1.5, color: 'var(--band-3)' },
] as const

export function glideslopeCall(gse: number): { error: 'H' | 'LO'; mag: 'little' | 'normal' | 'lot' } | null {
  if (gse > 1.5) return { error: 'H', mag: 'lot' }
  if (gse > 0.8) return { error: 'H', mag: 'normal' }
  if (gse > 0.4) return { error: 'H', mag: 'little' }
  if (gse < -0.9) return { error: 'LO', mag: 'lot' }
  if (gse < -0.6) return { error: 'LO', mag: 'normal' }
  if (gse < -0.3) return { error: 'LO', mag: 'little' }
  return null
}

export function lineupCall(lue: number): { error: 'LUR' | 'LUL'; mag: 'little' | 'normal' | 'lot' } | null {
  const error = lue > 0 ? 'LUR' : 'LUL'
  const a = Math.abs(lue)
  if (a > 3.0) return { error, mag: 'lot' }
  if (a > 1.0) return { error, mag: 'normal' }
  if (a > 0.5) return { error, mag: 'little' }
  return null
}

/** AAR letter bands from `grading::aar_grade`. */
export const AAR_LETTERS = [
  { letter: 'A', min: 4.5 },
  { letter: 'B', min: 3.75 },
  { letter: 'C', min: 3.0 },
  { letter: 'D', min: 2.0 },
  { letter: 'F', min: 0 },
] as const

/** Stability limits used by the AAR grade: SD in metres. */
export const AAR_STABILITY_LIMITS = { fore_aft: 2.0, lateral: 1.5, vertical: 1.5 } as const

/**
 * AAR grade from its measured components; mirrors `grading::aar_grade`.
 * Returns the 0..5 score, the letter and the calls that explain it.
 */
export function aarGrade(a: {
  contacts: number
  disconnects: number
  join_time_s: number | null
  fore_aft_sd: number
  lateral_sd: number
  vertical_sd: number
  precontact_closure_kts: number | null
  overshoot: boolean
}): { score: number; letter: string; calls: string[] } {
  const calls: string[] = []
  if (a.contacts === 0) return { score: 0, letter: 'F', calls: ['No contact made'] }
  let s = 5
  if (a.join_time_s !== null && a.join_time_s > 240) {
    s -= Math.min(1, (a.join_time_s - 240) / 120)
    calls.push(`Slow join-up (${a.join_time_s.toFixed(0)} s to first contact)`)
  }
  const extra = Math.max(0, a.disconnects - 1)
  if (extra > 0) {
    s -= Math.min(1.5, extra * 0.5)
    calls.push(`${a.disconnects} disconnects`)
  }
  const spread = (sd: number, lim: number) => Math.min(1, Math.max(0, (sd - lim) / lim))
  const st =
    spread(a.fore_aft_sd, 2.0) * 0.6 + spread(a.lateral_sd, 1.5) * 0.7 + spread(a.vertical_sd, 1.5) * 0.7
  if (st > 0.05) {
    s -= st
    const worst = [
      [a.fore_aft_sd / 2.0, 'fore-aft'],
      [a.lateral_sd / 1.5, 'lateral'],
      [a.vertical_sd / 1.5, 'vertical'],
    ].sort((x, y) => (y[0] as number) - (x[0] as number))
    calls.push(`Unstable in contact, mostly ${worst[0][1]}`)
  }
  if (a.precontact_closure_kts !== null && a.precontact_closure_kts > 5) {
    s -= 0.5
    calls.push(`Hot closure at pre-contact (${a.precontact_closure_kts.toFixed(0)} kt)`)
  }
  if (a.overshoot) {
    s -= 0.75
    calls.push('Overshot the tanker')
  }
  s = Math.min(5, Math.max(0, s))
  const letter = s >= 4.5 ? 'A' : s >= 3.75 ? 'B' : s >= 3.0 ? 'C' : s >= 2.0 ? 'D' : 'F'
  if (!calls.length) calls.push('Smooth join and stable in contact')
  return { score: s, letter, calls }
}
