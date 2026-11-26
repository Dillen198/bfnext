// Where the plot's objective names go: beside each symbol, to the right or
// else the left, most important objectives first. A name that would land on
// another name or on any symbol, or run off the plot, is left off; the symbol
// still shows.

export interface LabelItem {
  /** The symbol's centre, px. */
  x: number
  y: number
  /** The name's size, px. */
  w: number
  h: number
  /** Higher goes first. */
  pri: number
}

export type LabelSide = 'right' | 'left' | null

export interface Box { x0: number; x1: number; y0: number; y1: number }

const hits = (a: Box, b: Box) => a.x0 < b.x1 && b.x0 < a.x1 && a.y0 < b.y1 && b.y0 < a.y1

/** The side each item's name goes on, in the items' order. `gap` is from the
 *  symbol's centre to the name, `mark` the half-size of every symbol; a name
 *  has to fit inside `frame` when one is given. */
export function placeLabels(items: LabelItem[], gap: number, mark: number, frame?: Box): LabelSide[] {
  const marks: Box[] = items.map((it) => ({ x0: it.x - mark, x1: it.x + mark, y0: it.y - mark, y1: it.y + mark }))
  const placed: Box[] = []
  const out: LabelSide[] = items.map(() => null)
  const order = items.map((_, i) => i).sort((a, b) => items[b].pri - items[a].pri || a - b)
  for (const i of order) {
    const it = items[i]
    const y0 = it.y - it.h / 2
    const y1 = it.y + it.h / 2
    for (const side of ['right', 'left'] as const) {
      const box = side === 'right'
        ? { x0: it.x + gap, x1: it.x + gap + it.w, y0, y1 }
        : { x0: it.x - gap - it.w, x1: it.x - gap, y0, y1 }
      if (frame && (box.x0 < frame.x0 || box.x1 > frame.x1 || box.y0 < frame.y0 || box.y1 > frame.y1)) continue
      if (placed.some((p) => hits(box, p)) || marks.some((m, j) => j !== i && hits(box, m))) continue
      // Names keep a little clear of each other, or two can read as one.
      placed.push({ x0: box.x0 - 4, x1: box.x1 + 4, y0: box.y0 - 1, y1: box.y1 + 1 })
      out[i] = side
      break
    }
  }
  return out
}
