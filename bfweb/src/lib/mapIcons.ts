import { divIcon } from 'leaflet'
import ms from 'milsymbol'

export type IconStyle = 'dot' | 'nato' | 'russian'
export type Side = 'Red' | 'Blue' | 'Neutral'
export type ObjKind = string

// ── NATO SIDC mapping ────────────────────────────────────────────────
// APP-6D SIDC: identity (F=friendly, H=hostile, N=neutral) + battle dim + function
// Format: SXXX-affiliation-battleDimension-status-functionID

const NATO_IDENTITY: Record<Side, string> = {
  Blue:    'F', // Friendly
  Red:     'H', // Hostile
  Neutral: 'N', // Neutral
}

// SIDC codes per objective kind
// Using Ground installation codes (G) and Air (A) etc.
const NATO_SIDC: Record<string, string> = {
  // S-side-G-P-I (Ground, Present, Installation)
  Airbase:         'S_GP------H----', // Airfield
  FARP:            'S_GP------H----', // Forward Arming/Refueling Point → airfield
  FOB:             'S_GFIB----H----', // Forward Operating Base
  'Logistics Hub': 'S_GFSS----H----', // Supply
  'Naval Base':    'S_GP------H----', // Naval installation
  'Carrier Group': 'S_NB------H----', // Naval - Battle group
  Factory:         'S_GPI-----H----', // Installation
}

// Which SIDC to use based on kind, substituting the identity character
function buildSIDC(kind: ObjKind, side: Side): string {
  const template = NATO_SIDC[kind] ?? 'S_GP------H----'
  const id = NATO_IDENTITY[side]
  // SIDC char 1=S (warfighting), char 2=identity, rest from template
  return 'S' + id + template.slice(2)
}

// ── Russian military symbol SVGs ────────────────────────────────────
// Russian forces use different symbology: squares for ground, diamonds for units

const RU_SIDE_COLOR: Record<Side, { fill: string; stroke: string; text: string }> = {
  Blue:    { fill: '#1a3a6b', stroke: '#3b82f6', text: '#93c5fd' },
  Red:     { fill: '#6b1a1a', stroke: '#ef4444', text: '#fca5a5' },
  Neutral: { fill: '#2a2a2a', stroke: '#6b7280', text: '#9ca3af' },
}

function russianSVG(kind: ObjKind, side: Side, size = 36): string {
  const c = RU_SIDE_COLOR[side]

  // Shape: diamond for mobile/carrier, circle for airbase, square for ground
  const isAir     = kind === 'Airbase' || kind === 'FARP'
  const isNaval   = kind === 'Naval Base' || kind === 'Carrier Group'
  const isMobile  = kind === 'Carrier Group'

  const cx = size / 2
  const cy = size / 2
  const r  = size * 0.38

  let shape = ''
  let inner = ''

  if (isNaval) {
    // Diamond
    const d = r + 2
    shape = `<polygon points="${cx},${cy - d} ${cx + d},${cy} ${cx},${cy + d} ${cx - d},${cy}"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = isMobile
      ? `<text x="${cx}" y="${cy + 3.5}" text-anchor="middle" font-size="10" fill="${c.text}" font-family="Arial">⚓</text>`
      : `<text x="${cx}" y="${cy + 3.5}" text-anchor="middle" font-size="9" fill="${c.text}" font-family="Arial">⚓</text>`
  } else if (isAir) {
    // Circle with wings hint
    shape = `<circle cx="${cx}" cy="${cy}" r="${r}" fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = `<text x="${cx}" y="${cy + 3.5}" text-anchor="middle" font-size="11" fill="${c.text}" font-family="Arial">✈</text>`
  } else if (kind === 'Factory') {
    // Rectangle with gear
    const w = r * 1.8; const h = r * 1.4
    shape = `<rect x="${cx - w/2}" y="${cy - h/2}" width="${w}" height="${h}" rx="2"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = `<text x="${cx}" y="${cy + 3.5}" text-anchor="middle" font-size="11" fill="${c.text}" font-family="Arial">⚙</text>`
  } else {
    // Square (standard ground)
    const w = r * 1.8
    shape = `<rect x="${cx - w/2}" y="${cy - w/2}" width="${w}" height="${w}" rx="2"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    const sym = kind === 'FOB' ? '⬡' : kind === 'Logistics Hub' ? '▲' : '■'
    inner = `<text x="${cx}" y="${cy + 3.5}" text-anchor="middle" font-size="9" fill="${c.text}" font-family="Arial">${sym}</text>`
  }

  return `<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
    ${shape}
    ${inner}
  </svg>`
}

// ── Icon factory ─────────────────────────────────────────────────────

export function createMapIcon(kind: ObjKind, side: Side, style: IconStyle, selected = false) {
  const SIDE_COLOR: Record<Side, string> = {
    Red: '#ef4444', Blue: '#3b82f6', Neutral: '#6b7280',
  }

  if (style === 'dot') {
    // Return null — caller uses CircleMarker
    return null
  }

  if (style === 'nato') {
    const sidc = buildSIDC(kind, side)
    const sym = new ms.Symbol(sidc, {
      size: selected ? 30 : 24,
      frame: true,
      fill: true,
      colorMode: 'Light',
    })
    const svgStr = sym.asSVG()
    const anchor = sym.getAnchor()
    return divIcon({
      html: `<div style="filter: drop-shadow(0 2px 4px rgba(0,0,0,0.8))${selected ? ' drop-shadow(0 0 6px rgba(255,255,100,0.7))' : ''}">${svgStr}</div>`,
      iconSize: [sym.getSize().width, sym.getSize().height],
      iconAnchor: [anchor.x, anchor.y],
      className: '',
    })
  }

  if (style === 'russian') {
    const size = selected ? 40 : 32
    const svg = russianSVG(kind, side, size)
    const glow = selected ? `box-shadow: 0 0 8px 2px ${SIDE_COLOR[side]}80;` : ''
    return divIcon({
      html: `<div style="filter: drop-shadow(0 2px 6px rgba(0,0,0,0.9))${selected ? ` drop-shadow(0 0 8px ${SIDE_COLOR[side]}90)` : ''}; ${glow}">${svg}</div>`,
      iconSize: [size, size],
      iconAnchor: [size / 2, size / 2],
      className: '',
    })
  }

  return null
}
