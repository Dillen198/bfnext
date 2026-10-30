/**
 * The Vector Strike wheel's geometry and behaviour, ported from the cockpit
 * plugin (bfcockpit/Scripts/Hooks/bfcockpit.lua, wheel_module, and the
 * generated bfcockpit/Mods/Services/BFCockpit/Theme/wheel/layout.lua) so the
 * wiki's replica moves the way the real one does:
 *
 *  - the ring holds 2..10 equal slots, the first straight up, clockwise;
 *  - pointing at a slot that is a group opens its children as an arc of
 *    20-degree segments outside the ring, centred on the slot and snapped to
 *    a 5-degree grid;
 *  - clicking a group (on the ring or the arc) makes its children the ring;
 *  - the hub goes back a level, or closes the wheel at the top;
 *  - a destructive entry needs a second click within three seconds.
 *
 * Kept free of React so it can be tested without a browser.
 */
import type { WheelItem } from './parse'

/** Canvas units, from layout.lua `geom`. The canvas is CANVAS square. */
export const G = {
  canvas: 760,
  hubR: 92,
  rIn: 104,
  rOut: 232,
  gap: 2.2,
  subIn: 244,
  subOut: 344,
  arcStep: 5,
  childSpan: 20,
  childGap: 1.2,
  maxRing: 10,
  maxArc: 9,
  strip: { w: 640, h: 46 },
} as const

export const CENTRE = G.canvas / 2

// ── items into slots ─────────────────────────────────────────────────

/** Label words that pick an icon, first match wins (ICON_RULES). */
const ICON_RULES: [string, string][] = [
  ['unpack', 'crate_open'], ['unload', 'unload'], ['load', 'load'],
  ['destroy', 'destroy'], ['delete', 'destroy'], ['nearby', 'radar_sweep'],
  ['list', 'list'], ['spawn', 'spawn'], ['c-130', 'transport'],
  ['cargo', 'crate'], ['crate', 'crate'], ['base supply', 'hangar'],
  ['troop', 'troops'], ['csar', 'csar'], ['pilot', 'csar'],
  ['jtac', 'jtac'], ['target', 'target'], ['smoke', 'smoke'],
  ['objective', 'flag'], ['info', 'info'], ['action', 'bolt'],
  ['deploy', 'launcher'], ['flight', 'jet'], ['wingman', 'jet'],
  ['ewr', 'ewr'], ['gci', 'ewr'], ['recon', 'recon'],
  ['repair', 'wrench'], ['fuel', 'fuel'], ['weapon', 'bomb'],
  ['logistic', 'hangar'], ['ground', 'tank'], ['helo', 'helo'],
]

/** Every icon the wheel ships (layout.lua `icons`). */
export const ICONS = [
  'crate', 'crate_open', 'load', 'unload', 'spawn', 'destroy', 'radar_sweep', 'list', 'troops', 'csar',
  'jtac', 'target', 'smoke', 'flag', 'info', 'bolt', 'launcher', 'jet', 'helo', 'transport', 'ewr',
  'recon', 'wrench', 'fuel', 'bomb', 'hangar', 'tank', 'panel', 'chat', 'clock', 'compass', 'medal',
  'close', 'more', 'back', 'group', 'gear', 'star', 'generic', 'vector',
]

export function iconFor(label: string): string {
  const l = label.toLowerCase()
  for (const [word, icon] of ICON_RULES) if (l.includes(word)) return icon
  return 'generic'
}

const DESTRUCTIVE = ['destroy', 'delete', 'disband', 'abort', 'scuttle', 'remove', 'clear']

export function isDestructive(label: string): boolean {
  const l = label.toLowerCase()
  return DESTRUCTIVE.some(w => l.includes(w))
}

/** One slot as drawn. */
export interface Entry {
  label: string
  icon: string
  danger: boolean
  group: boolean
  children: WheelItem[]
  says?: string
  /** The hub's second line while this is highlighted. */
  hub: string
  /** Stable identity for the confirm-twice state. */
  key: string
}

function entryOf(item: WheelItem, trail: string[]): Entry {
  const path = [...trail, item.label]
  const group = !!item.items && item.items.length > 0
  const icon = item.icon && ICONS.includes(item.icon) ? item.icon : iconFor(item.label)
  return {
    label: item.label,
    icon: group && !item.icon && icon === 'generic' ? 'group' : icon,
    danger: item.danger ?? (!group && isDestructive(item.label)),
    group,
    children: item.items ?? [],
    says: item.says,
    hub: item.hub ?? (group ? `F10 > ${path.join(' > ')}` : path.join(' > ')),
    key: path.join('\u001f'),
  }
}

/** Items to slots, folding any past `max` into a MORE group (resolve()). */
export function resolve(items: WheelItem[], max: number, trail: string[] = []): Entry[] {
  if (items.length <= max) return items.map(it => entryOf(it, trail))
  const rest = items.slice(max - 1)
  const out = items.slice(0, max - 1).map(it => entryOf(it, trail))
  out.push({
    label: 'MORE', icon: 'more', danger: false, group: true, children: rest,
    hub: `${rest.length} more`, key: [...trail, '\u0000more'].join('\u001f'),
  })
  return out
}

// ── geometry ─────────────────────────────────────────────────────────

/** Screen degrees: 0 = right, clockwise, -90 = up. `i` counts from 0. */
export function slotMid(i: number, n: number): number {
  return -90 + (i * 360) / n
}

export function angleDiff(a: number, b: number): number {
  return Math.abs(((((a - b) + 540) % 360) + 360) % 360 - 180)
}

/** Where an open arc starts: centred on its owner, on the 5-degree grid. */
export function arcStart(owner: number, n: number, count: number): number {
  const mid = slotMid(owner, n)
  return Math.floor((mid - (count * G.childSpan) / 2) / G.arcStep + 0.5) * G.arcStep
}

export function polar(r: number, deg: number): [number, number] {
  const a = (deg * Math.PI) / 180
  return [CENTRE + r * Math.cos(a), CENTRE + r * Math.sin(a)]
}

/** An annular sector as an SVG path, `a0` to `a1` degrees clockwise. */
export function sectorPath(rIn: number, rOut: number, a0: number, a1: number): string {
  const large = a1 - a0 > 180 ? 1 : 0
  const [x0, y0] = polar(rOut, a0)
  const [x1, y1] = polar(rOut, a1)
  const [x2, y2] = polar(rIn, a1)
  const [x3, y3] = polar(rIn, a0)
  const f = (v: number) => v.toFixed(2)
  return (
    `M${f(x0)} ${f(y0)}A${rOut} ${rOut} 0 ${large} 1 ${f(x1)} ${f(y1)}` +
    `L${f(x2)} ${f(y2)}A${rIn} ${rIn} 0 ${large} 0 ${f(x3)} ${f(y3)}Z`
  )
}

/** The angular extent of ring slot `i` of `n`, less the gap between slots. */
export function ringSpan(i: number, n: number): [number, number] {
  const span = 360 / n
  const mid = slotMid(i, n)
  return [mid - span / 2 + G.gap / 2, mid + span / 2 - G.gap / 2]
}

/** The angular extent of arc segment `j` of an arc starting at `start`. */
export function arcSpan(start: number, j: number): [number, number] {
  const a0 = start + j * G.childSpan
  return [a0 + G.childGap / 2, a0 + G.childSpan - G.childGap / 2]
}

/** Ring labels are upper case, clipped with an ellipsis (short()). */
export function short(text: string, n: number): string {
  const t = text.toUpperCase()
  return t.length <= n ? t : `${t.slice(0, n - 1)}…`
}

export function clip(text: string, n: number): string {
  return text.length <= n ? text : `${text.slice(0, n - 1)}…`
}

// ── state ────────────────────────────────────────────────────────────

export interface Frame {
  title: string
  items: WheelItem[]
  /** Labels from the top down to this frame, for the hub's paths. */
  trail: string[]
}

export interface WheelState {
  open: boolean
  stack: Frame[]
  hlMain?: number
  hlArc?: number
  /** The open arc: whose children, and where it starts. */
  arc?: { owner: number; start: number }
  onHub: boolean
  /** The key of a destructive entry waiting for its second click. */
  armed?: string
  /** What the server said to the last command, shown under the wheel. */
  answer?: string
  /** What is driving the highlight; keys show the key hints in the hub. */
  source?: 'mouse' | 'keys'
}

export function initialWheel(items: WheelItem[], title = 'VECTOR STRIKE'): WheelState {
  return { open: true, stack: [{ title, items, trail: [] }], onHub: false }
}

export function ringEntries(s: WheelState): Entry[] {
  const f = s.stack[s.stack.length - 1]
  return resolve(f.items, G.maxRing, f.trail)
}

export function arcEntries(s: WheelState): Entry[] {
  if (!s.arc) return []
  const owner = ringEntries(s)[s.arc.owner]
  if (!owner) return []
  const f = s.stack[s.stack.length - 1]
  return resolve(owner.children, G.maxArc, owner.label === 'MORE' ? f.trail : [...f.trail, owner.label])
}

/** The arc for ring slot `i`, or none when it is not a group. */
function withArc(s: WheelState, i: number): WheelState {
  const ring = ringEntries(s)
  const e = ring[i]
  if (!e?.group || e.children.length === 0) return { ...s, arc: undefined }
  const count = Math.min(e.children.length, G.maxArc)
  return { ...s, arc: { owner: i, start: arcStart(i, ring.length, count) } }
}

function changed(a: WheelState, b: WheelState): WheelState {
  // Moving to another entry disarms a pending confirm (point_at()).
  if (a.hlMain !== b.hlMain || a.hlArc !== b.hlArc) return { ...b, armed: undefined }
  return b
}

export type Zone = { zone: 'hub' } | { zone: 'main'; i: number } | { zone: 'arc'; j: number } | { zone: 'none' }

/** Where a point is, in canvas units from the hub (hit()). */
export function hit(s: WheelState, x: number, y: number): Zone {
  const n = ringEntries(s).length
  const r = Math.hypot(x, y)
  const ang = (Math.atan2(y, x) * 180) / Math.PI
  if (r < G.rIn - 4) return { zone: 'hub' }
  if (s.arc && r > G.rOut + 4 && r <= G.subOut + 50) {
    const m = arcEntries(s).length
    for (let j = 0; j < m; j++) {
      const mid = s.arc.start + j * G.childSpan + G.childSpan / 2
      if (angleDiff(ang, mid) <= G.childSpan / 2) return { zone: 'arc', j }
    }
  }
  if (n === 0 || r > G.subOut + 50) return { zone: 'none' }
  let best = Infinity
  let bi = 0
  for (let i = 0; i < n; i++) {
    const d = angleDiff(ang, slotMid(i, n))
    if (d < best) {
      best = d
      bi = i
    }
  }
  return { zone: 'main', i: bi }
}

export type WheelAction =
  | { type: 'point'; zone: Zone; source: 'mouse' | 'keys' }
  | { type: 'leave' }
  | { type: 'click'; zone: Zone }
  | { type: 'back' }
  | { type: 'open' }
  | { type: 'disarm' }
  | { type: 'nav'; dir: 'next' | 'prev' | 'out' | 'into' }

function point(s: WheelState, zone: Zone, source: 'mouse' | 'keys'): WheelState {
  switch (zone.zone) {
    case 'main': {
      let t: WheelState = { ...s, hlMain: zone.i, hlArc: undefined, onHub: false, source }
      if (!s.arc || s.arc.owner !== zone.i) t = withArc(t, zone.i)
      return changed(s, t)
    }
    case 'arc':
      if (!s.arc) return s
      return changed(s, { ...s, hlArc: zone.j, hlMain: s.arc.owner, onHub: false, source })
    case 'hub':
      return changed(s, { ...s, hlMain: undefined, hlArc: undefined, arc: undefined, onHub: true, source })
    case 'none':
      return changed(s, { ...s, hlMain: undefined, hlArc: undefined, onHub: false })
  }
}

function push(s: WheelState, e: Entry): WheelState {
  const f = s.stack[s.stack.length - 1]
  const trail = e.label === 'MORE' ? f.trail : [...f.trail, e.label]
  return {
    ...s,
    stack: [...s.stack, { title: e.label, items: e.children, trail }],
    hlMain: undefined, hlArc: undefined, arc: undefined, armed: undefined, onHub: false,
  }
}

function fire(s: WheelState, e: Entry | undefined): WheelState {
  if (!e) return s
  if (e.group) return push(s, e)
  if (e.danger && s.armed !== e.key) return { ...s, armed: e.key }
  return { ...s, armed: undefined, answer: e.says }
}

function back(s: WheelState): WheelState {
  if (s.stack.length > 1) {
    return { ...s, stack: s.stack.slice(0, -1), hlMain: undefined, hlArc: undefined, arc: undefined, armed: undefined }
  }
  return { ...s, open: false, hlMain: undefined, hlArc: undefined, arc: undefined, armed: undefined, onHub: false }
}

function selectRing(s: WheelState, i: number): WheelState {
  const n = ringEntries(s).length
  if (n === 0) return s
  const k = ((i % n) + n) % n
  return point(s, { zone: 'main', i: k }, 'keys')
}

function selectArc(s: WheelState, j: number): WheelState {
  const m = arcEntries(s).length
  if (!s.arc || m === 0) return s
  const k = ((j % m) + m) % m
  return point(s, { zone: 'arc', j: k }, 'keys')
}

export function wheelReducer(s: WheelState, a: WheelAction): WheelState {
  switch (a.type) {
    case 'point':
      return point(s, a.zone, a.source)
    case 'leave':
      return point(s, { zone: 'none' }, s.source ?? 'mouse')
    case 'click': {
      const z = a.zone
      if (z.zone === 'hub') return back(s)
      if (z.zone === 'arc') return fire(s, arcEntries(s)[z.j])
      if (z.zone === 'main') {
        const e = ringEntries(s)[z.i]
        // A group's first click opens its arc (what pointing does with a
        // mouse); the next one, with the arc already showing, goes in.
        if (e?.group && s.arc?.owner !== z.i) return point(s, z, s.source ?? 'mouse')
        return fire({ ...s, hlMain: z.i, hlArc: undefined }, e)
      }
      return s
    }
    case 'back':
      return back(s)
    case 'open':
      return { ...initialWheel(s.stack[0].items, s.stack[0].title) }
    case 'disarm':
      return { ...s, armed: undefined }
    case 'nav':
      switch (a.dir) {
        case 'next':
          return s.hlArc !== undefined ? selectArc(s, s.hlArc + 1) : selectRing(s, (s.hlMain ?? -1) + 1)
        case 'prev':
          return s.hlArc !== undefined ? selectArc(s, s.hlArc - 1) : selectRing(s, (s.hlMain ?? 0) - 1)
        case 'out':
          if (s.hlMain === undefined) return selectRing(s, 0)
          if (s.hlArc === undefined && s.arc && s.arc.owner === s.hlMain) {
            return selectArc(s, Math.ceil(arcEntries(s).length / 2) - 1)
          }
          return s
        case 'into':
          return s.hlArc !== undefined && s.hlMain !== undefined ? selectRing(s, s.hlMain) : s
      }
  }
}

/** Keys to wheel actions: the plugin's "arrows" navigation set. */
export function wheelKey(key: string): WheelAction | 'fire' | null {
  switch (key) {
    case 'ArrowRight': return { type: 'nav', dir: 'next' }
    case 'ArrowLeft': return { type: 'nav', dir: 'prev' }
    case 'ArrowUp': return { type: 'nav', dir: 'out' }
    case 'ArrowDown': return { type: 'nav', dir: 'into' }
    case 'Backspace':
    case 'Escape': return { type: 'back' }
    case 'Enter':
    case ' ': return 'fire'
    default: return null
  }
}

export const NAV_HINT = '← → move  ·  ↑ arc  ·  enter fire  ·  backspace back'

/** The hub's three lines (render_wheel()). */
export function hubText(s: WheelState, subtitle = ''): { title: string; sub: string; hint: string; bad: boolean } {
  const ring = ringEntries(s)
  const arc = arcEntries(s)
  const cur = s.hlArc !== undefined ? arc[s.hlArc] : s.hlMain !== undefined ? ring[s.hlMain] : undefined
  const frame = s.stack[s.stack.length - 1]
  let title: string
  let sub: string
  if (s.armed && cur) {
    title = 'AGAIN TO CONFIRM'
    sub = short(cur.label, 24)
  } else if (cur) {
    const owner = s.hlArc !== undefined && s.hlMain !== undefined ? ring[s.hlMain] : undefined
    title = owner ? `${short(owner.label, 10)} › ${short(cur.label, 12)}` : short(cur.label, 22)
    sub = cur.hub
  } else if (s.onHub) {
    title = s.stack.length > 1 ? '‹ BACK' : 'CLOSE'
    sub = ''
  } else {
    title = short(frame.title, 22)
    sub = s.stack.length > 1 ? `F10 > ${frame.trail.join(' > ')}` : subtitle
  }
  let hint = ''
  if (s.source === 'keys') hint = NAV_HINT
  else if (cur) hint = 'click · or tap the key'
  return { title, sub: clip(sub, 34), hint, bad: !!s.armed && !!cur }
}
