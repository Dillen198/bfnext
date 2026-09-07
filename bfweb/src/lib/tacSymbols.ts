// TACMAP symbology — matched to peace-eye (pbzweihander/peace-eye):
// hollow framed milsymbol NATO symbols, its exact ColorMode palette, modern
// (2525D-style) SIDCs, and its bearing/coordinate formatting helpers.

// @ts-ignore – milsymbol v3 ships types but the declaration may be missing
import ms from 'milsymbol'
import { forward as mgrsForward } from 'mgrs'
import type { AirTrack, GroundContact, TacSide } from '../api'

// peace-eye's palette: ColorMode(civilian, friend, hostile, neutral, unknown[, suspect])
export const COLOR_MODE = ms.ColorMode('#ffffff', '#17c2f6', '#ff8080', '#FDE68A', '#ffffff', '#ff8080')
export const COL_FRIEND = '#17c2f6'
export const COL_HOSTILE = '#ff8080'
export const COL_UNKNOWN = '#ffffff'
export const COL_NEUTRAL = '#FDE68A'
// data-block accent colours (peace-eye: white alt / orange GS / cyan climb)
export const COL_ALT = '#ffffff'
export const COL_GS = '#f97316'
export const COL_CLIMB = '#22d3ee'

export type Disp = 'friendly' | 'hostile' | 'unknown'

export function airDisp(t: AirTrack): Disp {
  return t.iff === 'friendly' ? 'friendly' : t.iff === 'unknown' ? 'unknown' : 'hostile'
}
export function dispColor(d: Disp): string {
  return d === 'friendly' ? COL_FRIEND : d === 'unknown' ? COL_UNKNOWN : COL_HOSTILE
}
// 2525D standard-identity digit (SIDC position 4): 1=Unknown 3=Friend 6=Hostile.
function ident(d: Disp): string {
  return d === 'friendly' ? '3' : d === 'hostile' ? '6' : '1'
}

// ── SIDC builders (peace-eye getSidc, adapted to our typed feed) ───────
export function airSidc(t: AirTrack): string {
  // Generic air track frame (we rarely know the exact airframe). Disposition
  // drives the frame shape + colour; the frame alone reads as "air contact".
  return `100${ident(airDisp(t))}010000${'0000000000'}`
}

export function groundSidc(c: GroundContact): string {
  // Ground/naval intel — always the enemy laydown, so hostile ident.
  const id = '6'
  let set = '10'            // mobile ground
  let mainIcon = '000000'
  let modifier = '0000'
  switch (c.class) {
    case 'airdefense': mainIcon = '130100'; break            // Air Defense
    case 'armor':      mainIcon = '120500'; break            // Armor / Tank
    case 'artillery':  mainIcon = '120600'; break            // Field Artillery
    case 'infantry':   mainIcon = '121100'; break            // Infantry
    case 'naval':      set = '30'; mainIcon = '120000'; break
    case 'airbase':    set = '20'; mainIcon = '110000'; break // installation
    default: break
  }
  return `100${id}${set}0000${mainIcon}${modifier}`
}

export function bullSidc(side: TacSide): string {
  // Blue → Friend (cyan), Red → Hostile (salmon) — matches the map palette.
  const id = side === 'Red' ? '6' : side === 'Blue' ? '3' : '1'
  return `100${id}2500002102000000`
}

// ── Symbol factory (hollow frame, thick stroke — peace-eye's look) ─────
// milsymbol v3's `colorMode` only tints the fill; for a fill:false frame we
// recolour with `monoColor`. `color` defaults to the standard-identity hue.
const cache = new Map<string, { svg: string; w: number; h: number }>()
export function tacSymbol(sidc: string, size = 16, color?: string): {
  svg: string; w: number; h: number
} {
  const key = `${sidc}|${size}|${color ?? ''}`
  const hit = cache.get(key)
  if (hit) return hit
  const sym = new ms.Symbol(sidc, {
    size, frame: true, fill: false, strokeWidth: 8, infoSize: 100,
    ...(color ? { monoColor: color } : { colorMode: COLOR_MODE }),
  })
  const s = sym.getSize()
  const out = { svg: sym.asSVG() as string, w: s.width, h: s.height }
  cache.set(key, out)
  return out
}

// ── Bearing / range / coordinate helpers (peace-eye util.ts) ──────────
export function getCardinal(angle: number): string {
  const step = 45
  const a = ((angle % 360) + 360) % 360 + step / 2
  const dirs = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N']
  return dirs[Math.floor(a / step)]
}
/** `047E` — 3-digit bearing + cardinal, peace-eye style. */
export function fmtBearing(deg: number): string {
  const b = Math.round(((deg % 360) + 360) % 360)
  return `${b.toString().padStart(3, '0')}${getCardinal(b)}`
}

function dms(coord: number, size: number): string {
  const a = Math.abs(coord)
  const d = Math.floor(a)
  const mNot = (a - d) * 60
  const m = Math.floor(mNot)
  const s = Math.floor((mNot - m) * 60)
  return `${d.toString().padStart(size, '0')}°${m.toString().padStart(2, '0')}'${s.toString().padStart(2, '0')}"`
}
function ddm(coord: number, size: number): string {
  const a = Math.abs(coord)
  const d = Math.floor(a)
  return `${d.toString().padStart(size, '0')}°${((a - d) * 60).toFixed(3)}`
}
export function formatDMS(lat: number, lon: number): string {
  return `${lat >= 0 ? 'N' : 'S'}${dms(lat, 2)} ${lon >= 0 ? 'E' : 'W'}${dms(lon, 3)}`
}
export function formatDDM(lat: number, lon: number): string {
  return `${lat >= 0 ? 'N' : 'S'}${ddm(lat, 2)} ${lon >= 0 ? 'E' : 'W'}${ddm(lon, 3)}`
}
export function formatMGRS(lat: number, lon: number): string {
  try { return mgrsForward([lon, lat]) } catch { return '—' }
}
