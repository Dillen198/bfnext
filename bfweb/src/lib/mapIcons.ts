import { divIcon } from 'leaflet'
import ms from 'milsymbol'

export type IconStyle = 'dot' | 'nato' | 'russian'
export type Side = 'Red' | 'Blue' | 'Neutral'
export type ObjKind = string

// ── NATO unit sprite set (public/Nato/sprites) ─────────────────────────
// Real APP-6-style symbol images, keyed by the DCS UnitTag values bflib
// reports (see bfprotocols::cfg::UnitTag). First matching tag wins.
const SPRITE_TAG_PRIORITY: [tag: string, file: string][] = [
  ['Aircraft', 'fixedwing'],
  ['Helicopter', 'helicopter'],
  ['Armor', 'armor'],
  ['APC', 'mechinfantry'],
  ['Infantry', 'infantry'],
  ['Artillery', 'artillery'],
  ['Logistics', 'engineering'],
]

/** URL of a unit-type sprite for the given side/tags, or null if none matches
 *  (no Neutral set exists, and several tags — SAM, AAA, EWR, Boat — have no
 *  dedicated sprite yet, so callers should fall back to another icon style). */
export function spriteIconUrl(side: Side, tags: string[]): string | null {
  if (side === 'Neutral') return null
  const folder = side === 'Red' ? 'Nato_Hostile' : 'Nato_Friendly'
  for (const [tag, file] of SPRITE_TAG_PRIORITY) {
    if (tags.includes(tag)) return `/Nato/sprites/symbols/${folder}/${file}.png`
  }
  return null
}

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
  'Command Center': 'S_GFIC----H----', // Signal/command installation
}

// Which SIDC to use based on kind, substituting the identity character
function buildSIDC(kind: ObjKind, side: Side): string {
  const template = NATO_SIDC[kind] ?? 'S_GP------H----'
  const id = NATO_IDENTITY[side]
  // SIDC char 1=S (warfighting), char 2=identity, rest from template
  return 'S' + id + template.slice(2)
}

// ── Russian military symbol SVGs ────────────────────────────────────
// GOST-style: circle for airfields, square for fixed ground installations,
// diamond for mobile/naval units, hexagon for FOBs, triangle for supply.
// Glyphs are drawn as vector paths — never rely on emoji font rendering,
// which is inconsistent across browsers/platforms.

const RU_SIDE_COLOR: Record<Side, { fill: string; stroke: string; text: string }> = {
  Blue:    { fill: '#1a3a6b', stroke: '#3b82f6', text: '#93c5fd' },
  Red:     { fill: '#6b1a1a', stroke: '#ef4444', text: '#fca5a5' },
  Neutral: { fill: '#2a2a2a', stroke: '#6b7280', text: '#9ca3af' },
}

// Top-down aircraft silhouette, centered at (cx,cy), sized by r.
function glyphAircraft(cx: number, cy: number, r: number, stroke: string): string {
  const p = [
    [cx, cy - r], [cx + r * 0.15, cy - r * 0.3], [cx + r, cy + r * 0.05],
    [cx + r, cy + r * 0.28], [cx + r * 0.18, cy - r * 0.02], [cx + r * 0.22, cy + r * 0.75],
    [cx + r * 0.55, cy + r], [cx, cy + r * 0.72], [cx - r * 0.55, cy + r],
    [cx - r * 0.22, cy + r * 0.75], [cx - r * 0.18, cy - r * 0.02], [cx - r, cy + r * 0.28],
    [cx - r, cy + r * 0.05], [cx - r * 0.15, cy - r * 0.3],
  ].map(([x, y]) => `${x.toFixed(1)},${y.toFixed(1)}`).join(' ')
  return `<polygon points="${p}" fill="${stroke}" stroke="none"/>`
}

// Anchor drawn from primitives — ring, shaft, crossbar, two flukes.
function glyphAnchor(cx: number, cy: number, r: number, stroke: string): string {
  const ringY = cy - r * 0.55, ringR = r * 0.16
  const topY = cy - r * 0.4, botY = cy + r * 0.55
  const barY = cy - r * 0.05
  return `
    <circle cx="${cx}" cy="${ringY}" r="${ringR}" fill="none" stroke="${stroke}" stroke-width="1.3"/>
    <line x1="${cx}" y1="${topY}" x2="${cx}" y2="${botY}" stroke="${stroke}" stroke-width="1.3"/>
    <line x1="${cx - r * 0.35}" y1="${barY}" x2="${cx + r * 0.35}" y2="${barY}" stroke="${stroke}" stroke-width="1.1"/>
    <path d="M ${cx - r * 0.45} ${cy + r * 0.15} Q ${cx - r * 0.5} ${cy + r * 0.58} ${cx - r * 0.05} ${cy + r * 0.5}"
      fill="none" stroke="${stroke}" stroke-width="1.2"/>
    <path d="M ${cx + r * 0.45} ${cy + r * 0.15} Q ${cx + r * 0.5} ${cy + r * 0.58} ${cx + r * 0.05} ${cy + r * 0.5}"
      fill="none" stroke="${stroke}" stroke-width="1.2"/>`
}

// Gear (cog) built from an alternating inner/outer radius polygon.
function glyphGear(cx: number, cy: number, r: number, stroke: string, bg: string): string {
  const teeth = 8
  const outer = r * 0.9, inner = r * 0.62, hub = r * 0.28
  const pts: string[] = []
  for (let i = 0; i < teeth * 2; i++) {
    const ang = (Math.PI * i) / teeth
    const rad = i % 2 === 0 ? outer : inner
    pts.push(`${(cx + rad * Math.sin(ang)).toFixed(1)},${(cy - rad * Math.cos(ang)).toFixed(1)}`)
  }
  return `
    <polygon points="${pts.join(' ')}" fill="${stroke}" stroke="none"/>
    <circle cx="${cx}" cy="${cy}" r="${hub}" fill="${bg}"/>`
}

// Small upward supply arrow inside a triangle marker.
function glyphSupply(cx: number, cy: number, r: number, stroke: string): string {
  return `<path d="M ${cx} ${cy - r * 0.5} L ${cx} ${cy + r * 0.5} M ${cx - r * 0.32} ${cy - r * 0.1} L ${cx} ${cy - r * 0.5} L ${cx + r * 0.32} ${cy - r * 0.1}"
    fill="none" stroke="${stroke}" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"/>`
}

// Small watchtower glyph for FOBs.
function glyphTower(cx: number, cy: number, r: number, stroke: string): string {
  return `<path d="M ${cx - r * 0.35} ${cy + r * 0.5} L ${cx - r * 0.15} ${cy - r * 0.5} L ${cx + r * 0.15} ${cy - r * 0.5} L ${cx + r * 0.35} ${cy + r * 0.5}
    M ${cx - r * 0.25} ${cy + r * 0.15} L ${cx + r * 0.25} ${cy + r * 0.15}
    M ${cx - r * 0.2} ${cy - r * 0.15} L ${cx + r * 0.2} ${cy - r * 0.15}"
    fill="none" stroke="${stroke}" stroke-width="1.3" stroke-linecap="round"/>`
}

function russianSVG(kind: ObjKind, side: Side, size = 36): string {
  const c = RU_SIDE_COLOR[side]
  const cx = size / 2, cy = size / 2, r = size * 0.38

  let shape = ''
  let inner = ''

  if (kind === 'Airbase') {
    // Circle — fixed airfield
    shape = `<circle cx="${cx}" cy="${cy}" r="${r}" fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = glyphAircraft(cx, cy, r * 0.62, c.text)
  } else if (kind === 'FARP') {
    // Dashed circle — temporary/forward airfield
    shape = `<circle cx="${cx}" cy="${cy}" r="${r}" fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5" stroke-dasharray="3 2"/>`
    inner = glyphAircraft(cx, cy, r * 0.55, c.text)
  } else if (kind === 'Carrier Group') {
    // Diamond — mobile naval unit
    const d = r + 2
    shape = `<polygon points="${cx},${cy - d} ${cx + d},${cy} ${cx},${cy + d} ${cx - d},${cy}"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = glyphAnchor(cx, cy, r * 0.85, c.text)
  } else if (kind === 'Naval Base') {
    // Square — fixed naval installation
    const w = r * 1.8
    shape = `<rect x="${cx - w / 2}" y="${cy - w / 2}" width="${w}" height="${w}" rx="2"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = glyphAnchor(cx, cy, r * 0.8, c.text)
  } else if (kind === 'Factory') {
    // Rounded rectangle — industrial installation
    const w = r * 1.9; const h = r * 1.5
    shape = `<rect x="${cx - w / 2}" y="${cy - h / 2}" width="${w}" height="${h}" rx="2"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = glyphGear(cx, cy, r * 0.68, c.text, c.fill)
  } else if (kind === 'Logistics Hub') {
    // Triangle — supply point
    const d = r + 2
    shape = `<polygon points="${cx},${cy - d} ${cx + d * 0.95},${cy + d * 0.75} ${cx - d * 0.95},${cy + d * 0.75}"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = glyphSupply(cx, cy + r * 0.15, r * 0.6, c.text)
  } else if (kind === 'FOB') {
    // Hexagon — forward operating base
    const pts = Array.from({ length: 6 }, (_, i) => {
      const ang = Math.PI / 6 + (Math.PI * i) / 3
      return `${(cx + r * Math.cos(ang)).toFixed(1)},${(cy + r * Math.sin(ang)).toFixed(1)}`
    }).join(' ')
    shape = `<polygon points="${pts}" fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = glyphTower(cx, cy, r * 0.75, c.text)
  } else {
    // Square — generic ground installation
    const w = r * 1.8
    shape = `<rect x="${cx - w / 2}" y="${cy - w / 2}" width="${w}" height="${w}" rx="2"
      fill="${c.fill}" stroke="${c.stroke}" stroke-width="1.5"/>`
    inner = `<rect x="${cx - r * 0.3}" y="${cy - r * 0.3}" width="${r * 0.6}" height="${r * 0.6}" fill="${c.text}"/>`
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
      size: selected ? 24 : 18,
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
    const size = selected ? 32 : 26
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

// ── Radar / air-defense contact glyph ──────────────────────────────────
// SAM, AAA and EWR contacts have no matching sprite in public/Nato, so they
// used to fall back to the same generic ground blob as every other ground
// unit. Draw a small dish-on-mast icon instead so they read distinctly.
const RADAR_COLOR: Record<Side, string> = {
  Red: '#ef4444', Blue: '#3b82f6', Neutral: '#9ca3af',
}

export function radarGlyphSvg(side: Side, size = 22): { svg: string; width: number; height: number } {
  const c = RADAR_COLOR[side]
  const cx = size / 2, cy = size * 0.58, r = size * 0.42
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">
    <line x1="${cx}" y1="${cy}" x2="${cx}" y2="${size}" stroke="${c}" stroke-width="1.6"/>
    <path d="M ${cx - r} ${cy} A ${r} ${r} 0 0 1 ${cx + r} ${cy}" fill="none" stroke="${c}" stroke-width="1.8" stroke-linecap="round"/>
    <path d="M ${cx - r * 0.6} ${cy - r * 0.15} L ${cx} ${cy - r * 0.65} L ${cx + r * 0.6} ${cy - r * 0.15} Z" fill="${c}"/>
    <path d="M ${cx} ${cy - r * 0.45} L ${cx} 2" stroke="${c}" stroke-width="1.2"/>
    <circle cx="${cx}" cy="2" r="1.4" fill="${c}"/>
  </svg>`
  return { svg, width: size, height: size }
}
