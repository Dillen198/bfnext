// The replay's screen overlay: one canvas over the map (2D) or the theatre
// (3D). In 2D it draws everything -- icons, trails, lock lines, threat rings,
// radar sweeps, effects -- straight onto a canvas each frame, which is what
// keeps thousands of objects smooth (feeding them through MapLibre's GeoJSON
// sources re-tiles every source on a worker per frame). In both views it
// draws the Tacview-style labels, decluttered so they never pile up.

import { isWeapon, type Kind, type State } from './data'

export type SideKey = 'blue' | 'red' | 'other'

/** Tacview's palette, slightly muted for the dark map. */
export const SIDE_COLOR: Record<SideKey | 'focus' | 'target', string> = {
  blue: '#3d9bff',
  red: '#ff4747',
  other: '#e2c044',
  focus: '#b6f04a',
  target: '#ffffff',
}

export const sideKey = (c?: string | null): SideKey => (c === 'Blue' ? 'blue' : c === 'Red' ? 'red' : 'other')

/** One object as drawn this frame, in screen pixels. */
export interface Item {
  idx: number
  kind: Kind
  side: SideKey
  st: State
  x: number
  y: number
  focus: boolean
  target: boolean
  /** label lines (empty: no label) */
  line1: string
  line2: string
  /** draw order and label priority: higher wins */
  pri: number
}

export interface Polyline { pts: Float32Array; n: number; color: string; alpha: number; width: number; dash?: number[] }
export interface Ring { x: number; y: number; r: number; color: string; alpha: number; label?: string; dash?: boolean }
export interface Wedge { x: number; y: number; r: number; from: number; to: number; color: string }
export interface Text2D { x: number; y: number; text: string; color: string; alpha?: number; box?: boolean }

export interface Scene2D {
  items: Item[]
  lines: Polyline[]
  rings: Ring[]
  wedges: Wedge[]
  texts: Text2D[]
  /** draw labels at all (the focus and target always get one) */
  labels: boolean
  /** icon size multiplier */
  scale: number
}

// ── icons ──────────────────────────────────────────────────────────────

type Shape = 'jet' | 'helo' | 'missile' | 'bomb' | 'vehicle' | 'armor' | 'sam' | 'infantry' | 'static' | 'ship' | 'carrier'

const SHAPE: Partial<Record<Kind, Shape>> = {
  air: 'jet', helo: 'helo', missile: 'missile', bomb: 'bomb', rocket: 'missile', torpedo: 'missile',
  sam: 'sam', armor: 'armor', vehicle: 'vehicle', infantry: 'infantry', static: 'static', ship: 'ship', carrier: 'carrier',
  parachutist: 'infantry', misc: 'static',
}

/** Kinds whose icon turns with the heading. */
const ROTATES: Partial<Record<Kind, true>> = { air: true, helo: true, missile: true, bomb: true, rocket: true, torpedo: true, ship: true, carrier: true, vehicle: true, armor: true }

function drawShape(ctx: CanvasRenderingContext2D, shape: Shape, s: number) {
  const c = s / 2
  ctx.beginPath()
  switch (shape) {
    case 'jet':
      ctx.moveTo(c, s * 0.04)
      ctx.lineTo(c + s * 0.07, s * 0.36)
      ctx.lineTo(c + s * 0.44, s * 0.6)
      ctx.lineTo(c + s * 0.44, s * 0.68)
      ctx.lineTo(c + s * 0.08, s * 0.6)
      ctx.lineTo(c + s * 0.06, s * 0.8)
      ctx.lineTo(c + s * 0.2, s * 0.92)
      ctx.lineTo(c - s * 0.2, s * 0.92)
      ctx.lineTo(c - s * 0.06, s * 0.8)
      ctx.lineTo(c - s * 0.08, s * 0.6)
      ctx.lineTo(c - s * 0.44, s * 0.68)
      ctx.lineTo(c - s * 0.44, s * 0.6)
      ctx.lineTo(c - s * 0.07, s * 0.36)
      ctx.closePath()
      break
    case 'helo':
      ctx.ellipse(c, s * 0.42, s * 0.14, s * 0.22, 0, 0, Math.PI * 2)
      ctx.rect(c - s * 0.04, s * 0.6, s * 0.08, s * 0.32)
      ctx.rect(c - s * 0.42, s * 0.4, s * 0.84, s * 0.05)
      ctx.rect(c - s * 0.025, s * 0.02, s * 0.05, s * 0.8)
      break
    case 'missile':
      ctx.moveTo(c, s * 0.1)
      ctx.lineTo(c + s * 0.06, s * 0.3)
      ctx.lineTo(c + s * 0.06, s * 0.7)
      ctx.lineTo(c + s * 0.18, s * 0.9)
      ctx.lineTo(c - s * 0.18, s * 0.9)
      ctx.lineTo(c - s * 0.06, s * 0.7)
      ctx.lineTo(c - s * 0.06, s * 0.3)
      ctx.closePath()
      break
    case 'bomb':
      ctx.ellipse(c, c, s * 0.12, s * 0.26, 0, 0, Math.PI * 2)
      break
    case 'vehicle':
      ctx.rect(s * 0.3, s * 0.22, s * 0.4, s * 0.56)
      break
    case 'armor':
      ctx.rect(s * 0.26, s * 0.2, s * 0.48, s * 0.6)
      ctx.moveTo(c, s * 0.02)
      ctx.lineTo(c, s * 0.3)
      break
    case 'sam':
      ctx.moveTo(c, s * 0.12)
      ctx.lineTo(s * 0.86, s * 0.82)
      ctx.lineTo(s * 0.14, s * 0.82)
      ctx.closePath()
      break
    case 'infantry':
      ctx.arc(c, c, s * 0.14, 0, Math.PI * 2)
      break
    case 'static':
      ctx.rect(s * 0.34, s * 0.34, s * 0.32, s * 0.32)
      break
    case 'ship':
      ctx.moveTo(c, s * 0.04)
      ctx.lineTo(c + s * 0.14, s * 0.3)
      ctx.lineTo(c + s * 0.14, s * 0.94)
      ctx.lineTo(c - s * 0.14, s * 0.94)
      ctx.lineTo(c - s * 0.14, s * 0.3)
      ctx.closePath()
      break
    case 'carrier':
      ctx.moveTo(c - s * 0.1, s * 0.02)
      ctx.lineTo(c + s * 0.16, s * 0.02)
      ctx.lineTo(c + s * 0.16, s * 0.98)
      ctx.lineTo(c - s * 0.16, s * 0.98)
      ctx.closePath()
      break
  }
}

/** CSS px of an icon, by shape. */
const ICON_PX: Record<Shape, number> = {
  jet: 24, helo: 22, missile: 15, bomb: 12, vehicle: 16, armor: 16, sam: 17, infantry: 10, static: 12, ship: 24, carrier: 30,
}

const sprites = new Map<string, HTMLCanvasElement>()

function sprite(shape: Shape, color: string, dpr: number): HTMLCanvasElement {
  const key = `${shape}|${color}|${dpr}`
  let cv = sprites.get(key)
  if (cv) return cv
  const px = Math.ceil(ICON_PX[shape] * dpr)
  cv = document.createElement('canvas')
  cv.width = cv.height = px
  const ctx = cv.getContext('2d')!
  const pad = 2 * dpr
  ctx.translate(pad, pad)
  drawShape(ctx, shape, px - pad * 2)
  ctx.lineJoin = 'round'
  ctx.lineWidth = 2.6 * dpr
  ctx.strokeStyle = 'rgba(0,0,0,0.8)'
  ctx.stroke()
  if (shape === 'vehicle' || shape === 'sam') {
    ctx.lineWidth = 1.6 * dpr
    ctx.strokeStyle = color
    ctx.stroke()
  } else {
    ctx.fillStyle = color
    ctx.fill()
  }
  sprites.set(key, cv)
  return cv
}

// ── the overlay ────────────────────────────────────────────────────────

const LABEL_FONT = '600 10.5px "JetBrains Mono", ui-monospace, Menlo, Consolas, monospace'
const LABEL_FONT_SMALL = '500 9.5px "JetBrains Mono", ui-monospace, Menlo, Consolas, monospace'

export class Overlay {
  readonly el: HTMLCanvasElement
  private ctx: CanvasRenderingContext2D
  private w = 0
  private h = 0
  private dpr = 1
  /** label boxes placed this frame, for decluttering */
  private boxes: number[] = []
  private scale = 1

  constructor(parent: HTMLElement) {
    this.el = document.createElement('canvas')
    this.el.className = 'rp-overlay'
    parent.appendChild(this.el)
    this.ctx = this.el.getContext('2d')!
  }

  destroy() { this.el.remove() }

  /** Match the canvas to its box; call before drawing. */
  begin() {
    const r = this.el.getBoundingClientRect()
    const dpr = Math.min(window.devicePixelRatio || 1, 2)
    const w = Math.round(r.width), h = Math.round(r.height)
    if (w !== this.w || h !== this.h || dpr !== this.dpr) {
      this.w = w; this.h = h; this.dpr = dpr
      this.el.width = Math.max(1, Math.round(w * dpr))
      this.el.height = Math.max(1, Math.round(h * dpr))
    }
    const ctx = this.ctx
    ctx.setTransform(1, 0, 0, 1, 0, 0)
    ctx.clearRect(0, 0, this.el.width, this.el.height)
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    this.boxes.length = 0
  }

  get width() { return this.w }
  get height() { return this.h }

  /** Everything the 2D view shows. */
  draw2D(sc: Scene2D) {
    const ctx = this.ctx
    this.scale = sc.scale
    // Rings and sweeps first (under everything).
    for (const r of sc.rings) {
      if (r.r < 3 || r.x + r.r < 0 || r.y + r.r < 0 || r.x - r.r > this.w || r.y - r.r > this.h) continue
      ctx.globalAlpha = r.alpha
      ctx.strokeStyle = r.color
      ctx.lineWidth = 1.2
      ctx.setLineDash(r.dash ? [6, 5] : [])
      ctx.beginPath()
      ctx.arc(r.x, r.y, r.r, 0, Math.PI * 2)
      ctx.stroke()
      if (r.label && r.r > 30) {
        ctx.setLineDash([])
        ctx.font = LABEL_FONT_SMALL
        ctx.fillStyle = r.color
        ctx.fillText(r.label, r.x + 3, r.y - r.r - 3)
      }
    }
    ctx.setLineDash([])
    for (const wd of sc.wedges) {
      if (wd.r < 4) continue
      ctx.globalAlpha = 0.16
      ctx.fillStyle = wd.color
      ctx.beginPath()
      ctx.moveTo(wd.x, wd.y)
      ctx.arc(wd.x, wd.y, wd.r, wd.from, wd.to)
      ctx.closePath()
      ctx.fill()
      ctx.globalAlpha = 0.5
      ctx.strokeStyle = wd.color
      ctx.lineWidth = 1
      ctx.stroke()
    }
    // Trails and lines.
    ctx.lineCap = 'round'
    ctx.lineJoin = 'round'
    for (const l of sc.lines) {
      if (l.n < 2) continue
      ctx.globalAlpha = l.alpha
      ctx.strokeStyle = l.color
      ctx.lineWidth = l.width
      ctx.setLineDash(l.dash ?? [])
      ctx.beginPath()
      ctx.moveTo(l.pts[0], l.pts[1])
      for (let i = 1; i < l.n; i++) ctx.lineTo(l.pts[i * 2], l.pts[i * 2 + 1])
      ctx.stroke()
    }
    ctx.setLineDash([])
    ctx.globalAlpha = 1
    // Objects, low priority first so aircraft sit on top.
    const items = sc.items
    for (const it of items) this.drawItem(it)
    this.labels(items, sc.labels)
    for (const t of sc.texts) {
      ctx.font = LABEL_FONT
      if (t.box) {
        const w = ctx.measureText(t.text).width
        ctx.globalAlpha = 0.8
        ctx.fillStyle = '#000'
        ctx.fillRect(t.x - 4, t.y - 12, w + 8, 17)
      }
      ctx.globalAlpha = t.alpha ?? 1
      ctx.fillStyle = t.color
      ctx.fillText(t.text, t.x, t.y)
    }
    ctx.globalAlpha = 1
  }

  private drawItem(it: Item) {
    const ctx = this.ctx
    const { x, y } = it
    if (x < -40 || y < -40 || x > this.w + 40 || y > this.h + 40) return
    const color = it.focus ? SIDE_COLOR.focus : SIDE_COLOR[it.side]
    switch (it.kind) {
      case 'flare':
        ctx.fillStyle = 'rgba(255,240,200,0.35)'
        ctx.beginPath(); ctx.arc(x, y, 4, 0, Math.PI * 2); ctx.fill()
        ctx.fillStyle = '#fffbe8'
        ctx.beginPath(); ctx.arc(x, y, 1.6, 0, Math.PI * 2); ctx.fill()
        return
      case 'chaff': case 'decoy':
        ctx.fillStyle = 'rgba(200,205,215,0.8)'
        ctx.fillRect(x - 1.2, y - 1.2, 2.4, 2.4)
        return
      case 'shell': {
        const a = (it.st.hdg - 90) * (Math.PI / 180)
        ctx.strokeStyle = '#ffd36b'
        ctx.lineWidth = 1.4
        ctx.beginPath()
        ctx.moveTo(x - Math.cos(a) * 5, y - Math.sin(a) * 5)
        ctx.lineTo(x + Math.cos(a) * 2, y + Math.sin(a) * 2)
        ctx.stroke()
        return
      }
      case 'explosion': {
        const r = Math.max(4, it.st.ch?.Radius ?? 6)
        ctx.fillStyle = 'rgba(255,140,40,0.35)'
        ctx.strokeStyle = 'rgba(255,190,90,0.9)'
        ctx.lineWidth = 1.2
        ctx.beginPath(); ctx.arc(x, y, Math.min(r, 30), 0, Math.PI * 2); ctx.fill(); ctx.stroke()
        return
      }
      case 'bullseye':
        return
    }
    const shape = SHAPE[it.kind]
    if (!shape) return
    const img = sprite(shape, color, this.dpr * this.scale)
    const s = img.width / this.dpr
    if (ROTATES[it.kind]) {
      // A weapon points along its track (its recorded heading can be stale).
      const a = (isWeapon(it.kind) && it.st.gs > 30 && it.st.path ? it.st.path.trk : it.st.hdg) * (Math.PI / 180)
      ctx.setTransform(this.dpr * Math.cos(a), this.dpr * Math.sin(a), -this.dpr * Math.sin(a), this.dpr * Math.cos(a), x * this.dpr, y * this.dpr)
      ctx.drawImage(img, -s / 2, -s / 2, s, s)
      ctx.setTransform(this.dpr, 0, 0, this.dpr, 0, 0)
    } else {
      ctx.drawImage(img, x - s / 2, y - s / 2, s, s)
    }
    if (it.target) {
      // Tacview's target box (the followed object is told apart by its colour alone)
      ctx.strokeStyle = SIDE_COLOR.target
      ctx.lineWidth = 1.4
      ctx.strokeRect(x - 13, y - 13, 26, 26)
    }
  }

  /**
   * Tacview-style labels: a short leader, then name over type/level/speed.
   * Highest priority places first; a label that would overlap one already
   * placed is left out (the focus and the target always get theirs).
   */
  labels(items: Item[], all: boolean) {
    const ctx = this.ctx
    const order = items.filter(it => it.line1 && (all || it.focus || it.target))
    order.sort((a, b) => b.pri - a.pri)
    ctx.textBaseline = 'alphabetic'
    for (const it of order) {
      const { x, y } = it
      if (x < 0 || y < 0 || x > this.w || y > this.h) continue
      const big = it.focus || it.target
      ctx.font = LABEL_FONT
      const w1 = ctx.measureText(it.line1).width
      ctx.font = LABEL_FONT_SMALL
      const w2 = it.line2 ? ctx.measureText(it.line2).width : 0
      const bw = Math.max(w1, w2) + 4
      const bh = it.line2 ? 24 : 13
      const lx = x + 14, ly = y - 18
      if (!big && this.overlaps(lx, ly, bw, bh)) continue
      this.boxes.push(lx, ly, bw, bh)
      const color = it.focus ? SIDE_COLOR.focus : it.target ? SIDE_COLOR.target : SIDE_COLOR[it.side]
      // leader
      ctx.globalAlpha = 0.75
      ctx.strokeStyle = color
      ctx.lineWidth = 1
      ctx.beginPath()
      ctx.moveTo(x + 6, y - 6)
      ctx.lineTo(lx, ly + 3)
      ctx.stroke()
      ctx.globalAlpha = 1
      ctx.shadowColor = '#000'
      ctx.shadowBlur = 3
      ctx.font = LABEL_FONT
      ctx.fillStyle = color
      ctx.fillText(it.line1, lx + 2, ly + 10)
      if (it.line2) {
        ctx.font = LABEL_FONT_SMALL
        ctx.fillStyle = big ? '#e8ecdf' : 'rgba(225,230,215,0.85)'
        ctx.fillText(it.line2, lx + 2, ly + 22)
      }
      ctx.shadowBlur = 0
    }
  }

  private overlaps(x: number, y: number, w: number, h: number): boolean {
    const b = this.boxes
    for (let i = 0; i < b.length; i += 4) {
      if (x < b[i] + b[i + 2] && x + w > b[i] && y < b[i + 1] + b[i + 3] && y + h > b[i + 1]) return true
    }
    return false
  }
}
