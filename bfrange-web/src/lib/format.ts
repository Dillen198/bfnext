/** Display helpers: names, units, times, quality tones. */
import type { ResultKind, SpawnCategory, StationKind } from '../types'
import { FT } from './geo'

export const KIND_LABEL: Record<ResultKind, string> = {
  bomb: 'Bombing',
  strafe: 'Strafe',
  trap: 'Carrier pass',
  aar: 'Air refuelling',
  missile: 'Missile trainer',
  engagement: 'Engagement',
  anti_ship: 'Anti-ship',
  sling: 'Helo cargo',
  landing: 'Precision landing',
  troops: 'Troop drill',
  gunnery: 'Gunnery',
  cas: 'CAS / JTAC',
}

/** Four-letter plate used on compact cards. */
export const KIND_CODE: Record<ResultKind, string> = {
  bomb: 'BOMB',
  strafe: 'GUNS',
  trap: 'TRAP',
  aar: 'AAR',
  missile: 'MSL',
  engagement: 'ACM',
  anti_ship: 'SHIP',
  sling: 'SLNG',
  landing: 'LAND',
  troops: 'TRPS',
  gunnery: 'GNRY',
  cas: 'CAS',
}

export const CATEGORY_LABEL: Record<SpawnCategory, string> = {
  air_to_air: 'Air-to-air adversaries',
  air_to_ground: 'Air-to-ground targets',
  tanker: 'Tankers',
  naval: 'Naval',
  ground: 'Ground forces',
  helo: 'Helicopter drills',
  jtac: 'JTAC',
}

export const STATION_KIND_LABEL: Record<StationKind, string> = {
  bomb_circle: 'Bomb circle',
  strafe_pit: 'Strafe pit',
  tactical_array: 'Tactical array',
  convoy: 'Moving convoy',
  coord_target: 'Coordinate target',
  laser_target: 'Laser target',
  ship_target: 'Ship target',
  gunnery_lane: 'Gunnery lane',
  sam_site: 'SAM site',
}

const AIRFRAMES: Record<string, string> = {
  'FA-18C_hornet': 'F/A-18C',
  'F-14B': 'F-14B',
  'F-14A-135-GR': 'F-14A',
  'T-45': 'T-45C',
  'F-16C_50': 'F-16C',
  'A-10C_2': 'A-10C II',
  'A-10C': 'A-10C',
  AV8BNA: 'AV-8B',
  'F-15ESE': 'F-15E',
  'M-2000C': 'M-2000C',
  'AH-64D_BLK_II': 'AH-64D',
  'UH-1H': 'UH-1H',
  'Mi-8MT': 'Mi-8',
  'CH-47Fbl1': 'CH-47F',
  'Ka-50_3': 'Ka-50',
  SA342M: 'Gazelle',
  'JF-17': 'JF-17',
  'MiG-29A': 'MiG-29A',
  'Su-27': 'Su-27',
  'Su-33': 'Su-33',
  KC135MPRS: 'KC-135MPRS',
  'KC-135': 'KC-135',
  KC130: 'KC-130',
  'S-3B Tanker': 'S-3B',
  'IL-78M': 'IL-78M',
  'A-6E': 'A-6E',
  'CVN_73': 'CVN-73',
  'Stennis': 'CVN-74',
}

/** "FA-18C_hornet" -> "F/A-18C"; unknown types come back with `_` as spaces. */
export function airframe(t: string): string {
  return AIRFRAMES[t] ?? t.replace(/_/g, ' ')
}

/** "GBU_16" -> "GBU-16", "HYDRA_70_M151" -> "HYDRA-70-M151". */
export function weaponName(w: string, display?: string): string {
  if (display) return display
  return w.replace(/_/g, '-')
}

export const mToFt = (m: number) => m / FT
export const ftToM = (ft: number) => ft * FT

export function fmt(n: number | null | undefined, digits = 0): string {
  if (n === null || n === undefined || !Number.isFinite(n)) return '—'
  return n.toLocaleString('en-US', { minimumFractionDigits: digits, maximumFractionDigits: digits })
}

/** Signed, e.g. "+0.6". */
export function fmtSigned(n: number | null | undefined, digits = 1): string {
  if (n === null || n === undefined || !Number.isFinite(n)) return '—'
  const s = Math.abs(n).toFixed(digits)
  // no "−0.0": anything that rounds to zero is plain zero
  if (Number(s) === 0) return s
  return (n >= 0 ? '+' : '−') + s
}

export function pad3(deg: number): string {
  return String(Math.round(((deg % 360) + 360) % 360) % 360).padStart(3, '0')
}

/** Seconds -> "4:07" (or "1:02:03"). */
export function fmtClock(s: number | null | undefined): string {
  if (s === null || s === undefined || !Number.isFinite(s)) return '—'
  const t = Math.max(0, Math.round(s))
  const h = Math.floor(t / 3600)
  const m = Math.floor((t % 3600) / 60)
  const sec = t % 60
  const mm = h ? String(m).padStart(2, '0') : String(m)
  return (h ? `${h}:` : '') + `${mm}:${String(sec).padStart(2, '0')}`
}

/** Seconds -> "14.2 s" / "3 min". */
export function fmtSecs(s: number | null | undefined): string {
  if (s === null || s === undefined || !Number.isFinite(s)) return '—'
  if (s < 90) return `${s.toFixed(s < 10 ? 1 : 0)} s`
  return `${Math.round(s / 60)} min`
}

export function fmtAgo(iso: string, now = Date.now()): string {
  const d = (now - new Date(iso).getTime()) / 1000
  if (!Number.isFinite(d)) return ''
  if (d < 45) return 'just now'
  if (d < 90) return '1 min ago'
  if (d < 3600) return `${Math.round(d / 60)} min ago`
  if (d < 86400) return `${Math.round(d / 3600)} h ago`
  const days = Math.round(d / 86400)
  return days === 1 ? 'yesterday' : `${days} days ago`
}

export function fmtDate(iso: string): string {
  const d = new Date(iso)
  return d.toLocaleDateString('en-GB', { day: '2-digit', month: 'short', year: 'numeric' })
}

export function fmtDateTime(iso: string): string {
  const d = new Date(iso)
  return (
    d.toLocaleDateString('en-GB', { day: '2-digit', month: 'short' }) +
    ' ' +
    d.toLocaleTimeString('en-GB', { hour: '2-digit', minute: '2-digit', timeZone: 'UTC' }) +
    'Z'
  )
}

/** Tone for any of the quality enums (bomb, strafe, precision). */
export type Tone = 'top' | 'great' | 'good' | 'fair' | 'poor' | 'none'

export function qualityTone(q: string): Tone {
  switch (q) {
    case 'SHACK':
    case 'DEADEYE':
    case 'PERFECT': return 'top'
    case 'EXCELLENT': return 'great'
    case 'GOOD': return 'good'
    case 'INEFFECTIVE':
    case 'FAIR': return 'fair'
    case 'POOR': return 'poor'
    default: return 'none'
  }
}

/** Tone for a normalised 0..5 score. */
export function scoreTone(score: number | null | undefined): Tone {
  if (score === null || score === undefined) return 'none'
  if (score >= 4.75) return 'top'
  if (score >= 3.75) return 'great'
  if (score >= 2.75) return 'good'
  if (score >= 1.75) return 'fair'
  return 'poor'
}

export const TONE_VAR: Record<Tone, string> = {
  top: 'var(--tone-top)',
  great: 'var(--tone-great)',
  good: 'var(--tone-good)',
  fair: 'var(--tone-fair)',
  poor: 'var(--tone-poor)',
  none: 'var(--tone-none)',
}

/** Short pilot label for a UCID (never show a full UCID as a name). */
export function shortUcid(ucid: string): string {
  return ucid.length > 10 ? `${ucid.slice(0, 4)}…${ucid.slice(-4)}` : ucid
}
