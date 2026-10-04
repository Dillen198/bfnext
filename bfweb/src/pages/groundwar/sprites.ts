// Everything the ground-war screen draws as a picture rather than a shape:
// the top-down vehicle silhouettes (canvas, registered on the map so a symbol
// layer can rotate hundreds of them cheaply), the NATO company symbols
// (milsymbol), and the live players' airframes (inline SVG).
import ms from 'milsymbol'
import {
  Airbase, Carrier, CommandCenter, Factory, Farp, Fob, LogiHub, NavalBase, Sam, type IconComponent,
} from '@icons'
import type { Map as MlMap } from 'maplibre-gl'
import type { GroundKind, GroundRole, LivePlayer } from '../../api'
import { BONE, ROLES, SIDE_BRIGHT, type Side } from './theme'

// ── Vehicle silhouettes ──────────────────────────────────────────────────

const PX = 2 // device pixels per sprite pixel
const S = 32 // sprite edge, sprite pixels

function shade(hex: string, f: number): string {
  const n = parseInt(hex.slice(1), 16)
  const ch = (v: number) => Math.max(0, Math.min(255, Math.round(f < 0 ? v * (1 + f) : v + (255 - v) * f)))
  return `rgb(${ch((n >> 16) & 255)},${ch((n >> 8) & 255)},${ch(n & 255)})`
}

function rect(c: CanvasRenderingContext2D, x0: number, y0: number, x1: number, y1: number, fill: string) {
  c.fillStyle = fill
  c.fillRect(x0, y0, x1 - x0, y1 - y0)
  c.strokeRect(x0, y0, x1 - x0, y1 - y0)
}
function disc(c: CanvasRenderingContext2D, x: number, y: number, r: number, fill: string) {
  c.beginPath()
  c.arc(x, y, r, 0, Math.PI * 2)
  c.fillStyle = fill
  c.fill()
  c.stroke()
}

/** Draw one vehicle, nose up, into a 32x32 box centred on the vehicle. */
function drawVehicle(c: CanvasRenderingContext2D, role: GroundRole, body: string) {
  const dark = shade(body, -0.5)
  const deck = shade(body, 0.18)
  c.lineWidth = 0.8
  c.strokeStyle = 'rgba(0,0,0,0.85)'
  switch (role) {
    case 'tank':
      rect(c, 8.5, 7, 11, 27, dark)
      rect(c, 21, 7, 23.5, 27, dark)
      rect(c, 11, 7.5, 21, 26.5, body)
      disc(c, 16, 18, 4.4, deck)
      rect(c, 15.25, 1, 16.75, 14, dark)
      break
    case 'ifv':
      rect(c, 9.5, 8, 11.5, 26, dark)
      rect(c, 20.5, 8, 22.5, 26, dark)
      rect(c, 11.5, 8, 20.5, 26, body)
      rect(c, 13.5, 13, 18.5, 18.5, deck)
      rect(c, 15.4, 4.5, 16.6, 13, dark)
      break
    case 'apc':
      c.beginPath()
      c.moveTo(12, 6.5)
      c.lineTo(20, 6.5)
      c.lineTo(21.5, 10)
      c.lineTo(21.5, 26.5)
      c.lineTo(10.5, 26.5)
      c.lineTo(10.5, 10)
      c.closePath()
      c.fillStyle = body
      c.fill()
      c.stroke()
      rect(c, 13.5, 16, 18.5, 21, deck)
      for (const y of [10, 15.5, 21]) {
        rect(c, 9, y, 10.5, y + 3, dark)
        rect(c, 21.5, y, 23, y + 3, dark)
      }
      break
    case 'recon':
      rect(c, 11.5, 9, 20.5, 25, body)
      for (const y of [10.5, 20.5]) {
        rect(c, 10, y, 11.5, y + 3, dark)
        rect(c, 20.5, y, 22, y + 3, dark)
      }
      disc(c, 16, 16, 2.8, deck)
      rect(c, 15.5, 6, 16.5, 14, dark)
      break
    case 'truck':
      rect(c, 11.5, 5.5, 20.5, 11, deck)
      rect(c, 11, 12.5, 21, 27.5, body)
      c.beginPath()
      for (const y of [16.5, 20.5, 24.5]) {
        c.moveTo(11.5, y)
        c.lineTo(20.5, y)
      }
      c.strokeStyle = 'rgba(0,0,0,0.4)'
      c.stroke()
      break
    case 'aaa':
      rect(c, 9.5, 8, 11.5, 26, dark)
      rect(c, 20.5, 8, 22.5, 26, dark)
      rect(c, 11.5, 8, 20.5, 26, body)
      rect(c, 12.5, 12.5, 19.5, 19.5, deck)
      rect(c, 13.2, 2, 14.4, 13, dark)
      rect(c, 17.6, 2, 18.8, 13, dark)
      c.beginPath()
      c.arc(16, 24, 3.4, Math.PI * 1.1, Math.PI * 1.9)
      c.lineWidth = 1.4
      c.strokeStyle = deck
      c.stroke()
      break
    case 'sam':
      rect(c, 9.5, 6.5, 11.5, 27, dark)
      rect(c, 20.5, 6.5, 22.5, 27, dark)
      rect(c, 11.5, 6.5, 20.5, 27, body)
      rect(c, 12.5, 11, 19.5, 24, deck)
      for (const [x, y] of [[14.2, 14], [17.8, 14], [14.2, 20.5], [17.8, 20.5]] as const) disc(c, x, y, 1.5, dark)
      break
    case 'artillery':
      rect(c, 9.5, 10, 11.5, 28, dark)
      rect(c, 20.5, 10, 22.5, 28, dark)
      rect(c, 11.5, 10, 20.5, 28, body)
      rect(c, 12.5, 14, 19.5, 23, deck)
      rect(c, 15.2, 0.5, 16.8, 15, dark)
      break
    case 'infantry':
      for (const [x, y] of [[16, 10], [11.5, 16], [20.5, 16], [16, 22]] as const) disc(c, x, y, 2.4, body)
      break
  }
}

const spriteCanvases: Record<string, HTMLCanvasElement> = {}

function spriteCanvas(role: GroundRole, side: Side, broken: boolean): HTMLCanvasElement {
  const key = `${role}-${side}-${broken ? 'b' : 'n'}`
  const hit = spriteCanvases[key]
  if (hit) return hit
  const cv = document.createElement('canvas')
  cv.width = S * PX
  cv.height = S * PX
  const c = cv.getContext('2d')
  if (c) {
    c.scale(PX, PX)
    c.shadowColor = 'rgba(0,0,0,0.75)'
    c.shadowBlur = 2.5
    drawVehicle(c, role, broken ? '#9a9a90' : SIDE_BRIGHT[side])
  }
  spriteCanvases[key] = cv
  return cv
}

export const vehicleImageId = (role: GroundRole, side: Side, broken: boolean): string =>
  `gwv-${role}-${side}-${broken ? 'b' : 'n'}`

function addVehicleImage(map: MlMap, id: string): boolean {
  const m = /^gwv-(\w+)-(Blue|Red)-(b|n)$/.exec(id)
  if (!m) return false
  const role = m[1] as GroundRole
  if (!ROLES.includes(role)) return false
  const cv = spriteCanvas(role, m[2] as Side, m[3] === 'b')
  const c = cv.getContext('2d')
  if (!c || map.hasImage(id)) return true
  map.addImage(id, c.getImageData(0, 0, cv.width, cv.height), { pixelRatio: PX })
  return true
}

/** Register every vehicle sprite now, and any the style asks for later (a
 *  style change drops images added at runtime). */
export function installVehicleImages(map: MlMap): () => void {
  for (const role of ROLES) {
    for (const side of ['Blue', 'Red'] as const) {
      addVehicleImage(map, vehicleImageId(role, side, false))
      addVehicleImage(map, vehicleImageId(role, side, true))
    }
  }
  const onMissing = (e: { id: string }) => {
    addVehicleImage(map, e.id)
  }
  map.on('styleimagemissing', onMissing)
  return () => {
    map.off('styleimagemissing', onMissing)
  }
}

const spriteUrls: Record<string, string> = {}
/** The same silhouette as an <img> source, for the composition chips. */
export function vehicleSpriteUrl(role: GroundRole, side: Side): string {
  const key = `${role}-${side}`
  if (!spriteUrls[key]) spriteUrls[key] = spriteCanvas(role, side, false).toDataURL()
  return spriteUrls[key]
}

// ── NATO symbols ─────────────────────────────────────────────────────────

export interface Sym {
  url: string
  w: number
  h: number
  /** Where the symbol's own centre is inside the image. */
  ax: number
  ay: number
}

const FUNCTION: Record<GroundKind, string> = {
  armour: 'UCA---',
  mechanised: 'UCIZ--',
  motorised: 'UCIM--',
  infantry: 'UCI---',
}

export interface SymOpts {
  kind: GroundKind
  hostile: boolean
  /** Dashed frame: anticipated / last-seen. */
  ghost?: boolean
  company?: boolean
  size: number
  fill: string
  designation?: string
  direction?: number | null
  reduced?: boolean
  info?: string
}

const symCache = new Map<string, Sym>()

export function natoSymbol(o: SymOpts): Sym {
  const dir = o.direction == null ? null : Math.round(o.direction / 15) * 15
  const key = `${o.kind}|${o.hostile}|${o.ghost}|${o.company}|${o.size}|${o.fill}|${o.designation ?? ''}|${dir}|${o.reduced}|${o.info ?? ''}`
  const hit = symCache.get(key)
  if (hit) return hit
  const sidc = `S${o.hostile ? 'H' : 'F'}G${o.ghost ? 'A' : 'P'}${FUNCTION[o.kind] ?? 'UC----'}-${o.company ? 'E' : '-'}---`
  const sym = new ms.Symbol(sidc, {
    size: o.size,
    fill: true,
    frame: true,
    fillColor: o.fill,
    fillOpacity: o.ghost ? 0.35 : 1,
    infoColor: BONE,
    infoSize: 46,
    infoOutlineColor: 'rgba(0,0,0,0.9)',
    infoOutlineWidth: 4,
    outlineColor: 'rgba(0,0,0,0.75)',
    outlineWidth: 3,
    // milsymbol measures every text field it is given, so an undefined one throws.
    ...(o.designation ? { uniqueDesignation: o.designation } : {}),
    ...(o.reduced ? { reinforcedReduced: '(-)' } : {}),
    ...(o.info ? { additionalInformation: o.info } : {}),
    ...(dir != null ? { direction: dir } : {}),
  })
  const size = sym.getSize()
  const anchor = sym.getAnchor()
  const out: Sym = {
    url: `data:image/svg+xml;utf8,${encodeURIComponent(sym.asSVG())}`,
    w: size.width,
    h: size.height,
    ax: anchor.x,
    ay: anchor.y,
  }
  if (symCache.size > 600) symCache.clear()
  symCache.set(key, out)
  return out
}

/** "1st Mech Coy (Senaki)" -> "1 MECH". */
export function shortName(name: string): string {
  const base = name.split(' (')[0]
  const m = /^(\d+)(?:st|nd|rd|th)?\s+(\w+)/i.exec(base)
  if (m) return `${m[1]} ${m[2].toUpperCase()}`
  return base.toUpperCase().slice(0, 10)
}

// ── Live players ─────────────────────────────────────────────────────────

const PLAYER_PATH: Record<LivePlayer['category'], string> = {
  plane:
    'M12 1.2 13.3 6.8 21.8 12.6v1.8l-8.5-2.4-.4 6.2 3.1 2.4v1.5L12 21.2 8 22.1v-1.5l3.1-2.4-.4-6.2-8.5 2.4v-1.8l8.5-5.8z',
  helicopter:
    'M12 4.6c1.9 0 3.1 1.7 3.1 4.3 0 2.2-1 3.6-2.3 4.1v6.6h2.4v1.4H8.8v-1.4h2.4V13C9.9 12.5 8.9 11.1 8.9 8.9c0-2.6 1.2-4.3 3.1-4.3z',
  ground: 'M8 6.5h8v14H8zM11.2 1.5h1.6v8h-1.6z',
  ship: 'M12 1.5 16 8v13.5H8V8z',
}

export function playerSvg(cat: LivePlayer['category'], fill: string): string {
  const rotor =
    cat === 'helicopter'
      ? '<circle cx="12" cy="9" r="8.4" fill="none" stroke="rgba(255,255,255,0.45)" stroke-width="0.8" stroke-dasharray="2 1.6"/>'
      : ''
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="26" height="26">${rotor}<path d="${PLAYER_PATH[cat] ?? PLAYER_PATH.plane}" fill="${fill}" stroke="rgba(0,0,0,0.85)" stroke-width="1.1" stroke-linejoin="round"/></svg>`
}

// ── Base glyphs, by objective kind ───────────────────────────────────────

export const OBJ_ICON: Record<string, IconComponent> = {
  airbase: Airbase,
  fob: Fob,
  farp: Farp,
  logistics: LogiHub,
  factory: Factory,
  naval: NavalBase,
  carrier: Carrier,
  command: CommandCenter,
  sam: Sam,
}
