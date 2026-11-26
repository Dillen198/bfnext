/** The kneeboard's two inks, and the base tiles each is drawn on. */
import { BASE_TILES } from './basemap'

export type KbTheme = 'light' | 'dark'

/**
 * The base each card is drawn on: the ocean chart on paper (coastlines and
 * reefs in plain colour), satellite on the night card (dark, the islands
 * plain to see), each with its place names.
 */
export const TILES: Record<KbTheme, { base: string; names?: string; maxzoom: number }> = {
  light: { base: BASE_TILES.ocean.light, names: BASE_TILES.ocean.names, maxzoom: BASE_TILES.ocean.maxzoom },
  dark: { base: BASE_TILES.satellite.dark, names: BASE_TILES.satellite.names, maxzoom: 18 },
}

export interface Ink {
  paper: string
  ink: string
  dim: string
  grid: string
  halo: string
  wash: string
  red: string
  teal: string
  amber: string
  sea: string
}

export const KB_INK: Record<KbTheme, Ink> = {
  light: {
    paper: '#ffffff', ink: '#11161c', dim: '#4a5562', grid: 'rgba(17,22,28,0.2)', halo: '#ffffff',
    wash: 'rgba(255,255,255,0.22)', red: '#c8102e', teal: '#00796d', amber: '#a85f00', sea: '#1d5fbf',
  },
  dark: {
    paper: '#0b0f14', ink: '#dbe2ea', dim: '#98a4b1', grid: 'rgba(219,226,234,0.16)', halo: '#0b0f14',
    wash: 'rgba(11,15,20,0.32)', red: '#ff5a4f', teal: '#2fd1bf', amber: '#ffb23e', sea: '#6aa8ff',
  },
}

export const KB_SANS = "Archivo, 'Segoe UI', Arial, sans-serif"
export const KB_MONO = "'IBM Plex Mono', Consolas, 'Courier New', monospace"

/**
 * A sector's F10 colour in this ink: on paper the light colours are taken
 * down (the white carrier and pattern areas to a dark grey), on black only
 * the pure whites are softened.
 */
export function sectorInk(hex: string, t: KbTheme): string {
  const m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(hex)
  if (!m) return hex
  const [r, g, b] = [m[1], m[2], m[3]].map(h => parseInt(h, 16))
  const lum = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255
  if (t === 'dark') return lum > 0.9 ? '#cfd6de' : hex
  const k = lum > 0.85 ? 0.3 : 1 - 0.55 * lum
  const c = (v: number) => Math.round(v * k).toString(16).padStart(2, '0')
  return `#${c(r)}${c(g)}${c(b)}`
}
