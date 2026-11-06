/**
 * Pure formatting and classification helpers shared across pages.
 *
 * These lived as private functions inside Dashboard.tsx / Pilots.tsx, where
 * nothing could test them and two pages had drifted into slightly different
 * copies of the same "what kind of thing is this target" regex. Everything
 * here is deliberately side-effect free and independent of React, so
 * `format.test.ts` can cover the arithmetic that silently produces wrong
 * numbers on screen.
 */

/** Kill/death ratio. Infinite when a pilot has kills and no deaths, which is
 *  a real state on a fresh campaign and must not read as 0.00. */
export function kd(air: number, ground: number, deaths: number): string {
  const k = air + ground
  return deaths > 0 ? (k / deaths).toFixed(2) : k > 0 ? '∞' : '0.00'
}

/** Landings per sortie -- the survival figure. Undefined with no sorties. */
export function sl(sorties: number, landings: number): string {
  return sorties > 0 ? (landings / sorties).toFixed(2) : '—'
}

/** Decimal hours as "3h 05m". Rounds to the nearest minute first, so 1.999h
 *  reads 2h 00m rather than 1h 60m. */
export function fmtHours(h: number): string {
  const total = Math.round(h * 60)
  const hh = Math.floor(total / 60)
  const mm = total % 60
  return `${hh}h ${mm.toString().padStart(2, '0')}m`
}

/** Seconds as "2h 7m" / "7m". Non-positive is unknown, not zero. */
export function fmtDuration(secs: number): string {
  if (secs <= 0) return '—'
  const h = Math.floor(secs / 3600)
  const m = Math.floor((secs % 3600) / 60)
  if (h > 0) return `${h}h ${m}m`
  return `${m}m`
}

/** Wall clock in Zulu. The trailing Z is only honest because of timeZone --
 *  without it this renders the viewer's local clock and still says "Z". */
export function fmtTimeZ(iso: string): string {
  return new Date(iso).toLocaleTimeString('en-US', {
    hour: '2-digit', minute: '2-digit', hour12: false, timeZone: 'UTC',
  }) + 'Z'
}

/** Relative age, coarsening as it gets older. */
export function ago(then: Date | string, now: number = Date.now()): string {
  const t = typeof then === 'string' ? new Date(then) : then
  const mins = Math.max(0, Math.round((now - t.getTime()) / 60000))
  if (mins < 60) return `${mins}m ago`
  const hrs = Math.floor(mins / 60)
  if (hrs < 24) return `${hrs}h ago`
  return `${Math.floor(hrs / 24)}d ago`
}

const COMPASS = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'] as const

/** Meteorological degrees to an 8-point compass label. Wraps past 360. */
export function windDir(deg: number): string {
  const i = Math.round((((deg % 360) + 360) % 360) / 45) % 8
  return COMPASS[i]
}

/** Visibility in metres. DCS reports 9999+ for "unlimited". */
export function visStr(m: number | null | undefined): string {
  if (!m) return '—'
  return m >= 9999 ? '10KM+' : `${(m / 1000).toFixed(1)}KM`
}

/** Cloud base in metres to feet, rounded to the nearest 100. */
export function cloudStr(m: number): string {
  if (!m || m === 0) return 'CLEAR'
  const ft = Math.round((m * 3.281) / 100) * 100
  return `${ft.toLocaleString()}FT`
}

// ── Target classification ──────────────────────────────────────────────────

export type TargetClass = 'AIR' | 'HELO' | 'NAVAL' | 'ARMOR' | 'AD' | 'VEH' | 'INF' | 'GND'

/**
 * Bucket a DCS unit type name into a broad category.
 *
 * Ordering and word anchors are both load-bearing here, and the unit tests
 * pin the three cases that actually bit:
 *   - "SA-" prefixes the SA342M Gazelle AND the SA-10 battery, so helicopters
 *     are matched on airframe names before the air-defence sweep runs.
 *   - an unanchored /a-10/ matches the "a-10" inside "sa-10", filing a SAM
 *     site as a Warthog.
 *   - the DCS name is "FA-18C_hornet", which /\bf-\d/ does not match at all.
 */
export function classifyTargetClass(targetType: string | null | undefined): TargetClass {
  const t = (targetType ?? '').toLowerCase()
  if (!t) return 'GND'
  if (/\bmi-\d|\buh-\d|\bah-\d|\bch-\d|\bka-\d|sa-?342|heli|huey|apache|blackhawk|chinook|hind|\bhip\b|havoc|hokum|seahawk|lynx|gazelle|rotary/.test(t)) return 'HELO'
  if (/\bf-?1[4-8]|\bfa-?18|\bf\/a-?18|\bmig-\d|\bsu-\d|\ba-10\b|\bav-8|tornado|rafale|gripen|mirage|harrier|phantom|hornet|viper|eagle|flanker|fulcrum|frogfoot|aircraft|plane|\bjet\b|fighter|bomber|transport|tanker|awacs/.test(t)) return 'AIR'
  if (/ship|naval|carrier|frigate|cruiser|destroyer|corvette|\bcvn/.test(t)) return 'NAVAL'
  if (/\bsa-\d|\bsam\b|patriot|radar|\baaa\b|s-300|s-400|\bbuk\b|\btor\b|\bosa\b|shilka|tunguska|pantsir|roland|rapier|\bhq-\d/.test(t)) return 'AD'
  if (/\bt-\d{2}|tank|armou?r|\bapc\b|\bifv\b|\bbmp|\bbtr|bradley|abrams|leopard|challenger|merkava/.test(t)) return 'ARMOR'
  if (/truck|supply|ural|kamaz|vehicle|\bcar\b|logistic|hemtt|\bzil\b|\bgaz\b/.test(t)) return 'VEH'
  if (/infantry|soldier|troop|manpad|stinger|igla|\bak\b|\brpg\b/.test(t)) return 'INF'
  return 'GND'
}

/** Display colour per category, as a CSS custom property reference. */
export const TARGET_CLASS_COLOR: Record<TargetClass, string> = {
  AIR:   'var(--blue)',
  HELO:  'var(--cyan)',
  NAVAL: 'var(--cyan)',
  ARMOR: 'var(--orange)',
  AD:    'var(--purple)',
  VEH:   'var(--yellow)',
  INF:   'var(--accent)',
  GND:   'var(--text-dim)',
}
