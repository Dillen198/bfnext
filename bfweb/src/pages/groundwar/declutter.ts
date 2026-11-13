// Which base names fit on screen. Bases that sit close together (an airfield
// and the SAM site beside it, a FOB in a town) wrote their names over each
// other; now each name is placed in turn, most important first, and one that
// would land on a name already placed -- or on another base's plate -- is
// left off until the map is zoomed in far enough for it to fit.
import type { GroundObjective } from '../../api'

export interface Pt { x: number; y: number }
interface Rect { x0: number; y0: number; x1: number; y1: number }

const PLATE = 26
/** Base label metrics, matching `.gw-obj-name` (12px display face). */
const CHAR_W = 7.2
const LABEL_H = 15
const PAD = 2

/** "LATAKIA - SA-2 (RED)" -> "LATAKIA - SA-2": the colour already says whose. */
export function displayName(name: string): string {
  return name.replace(/\s*\((red|blue|neutral)\)\s*$/i, '').toUpperCase()
}

function hit(a: Rect, b: Rect): boolean {
  return a.x0 < b.x1 && b.x0 < a.x1 && a.y0 < b.y1 && b.y0 < a.y1
}

function priority(o: GroundObjective, side: string, selected: number | null): number {
  return (o.id === selected ? 1000 : 0)
    + (o.being_captured ? 50 : 0)
    + (o.owner === side ? 20 : 0)
    + (o.kind === 'airbase' ? 10 : 0)
    + (o.threatened ? 5 : 0)
}

/**
 * The ids of the bases whose names to draw. `project` gives a base's plate
 * centre in screen pixels (null when it is off screen, which never blocks).
 */
export function visibleNames(
  objs: GroundObjective[],
  project: (o: GroundObjective) => Pt | null,
  side: string,
  selected: number | null,
): Set<number> {
  const at = new Map<number, Pt>()
  for (const o of objs) {
    const p = project(o)
    if (p) at.set(o.id, p)
  }
  const plates = new Map<number, Rect>()
  for (const [id, p] of at) {
    plates.set(id, { x0: p.x - PLATE / 2, y0: p.y - PLATE / 2, x1: p.x + PLATE / 2, y1: p.y + PLATE / 2 })
  }
  const placed: Rect[] = []
  const shown = new Set<number>()
  const order = [...objs].sort((a, b) => priority(b, side, selected) - priority(a, side, selected))
  for (const o of order) {
    const p = at.get(o.id)
    if (!p) {
      // Off screen: nothing to collide with; draw it so it is there on a pan.
      shown.add(o.id)
      continue
    }
    const w = displayName(o.name).length * CHAR_W + 2 * PAD
    const r: Rect = { x0: p.x - w / 2, y0: p.y + PLATE / 2, x1: p.x + w / 2, y1: p.y + PLATE / 2 + LABEL_H }
    const blocked = placed.some((q) => hit(r, q))
      || [...plates].some(([id, q]) => id !== o.id && hit(r, q))
    if (blocked && o.id !== selected) continue
    placed.push(r)
    shown.add(o.id)
  }
  return shown
}
