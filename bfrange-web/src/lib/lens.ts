/**
 * The IFLOLS ("the lens"): twelve source cells, the ball running about
 * +-0.8 degrees from the top of the source to the bottom, the bottom two
 * cells red. On glideslope the ball straddles the datum, lighting the two
 * middle cells. The same numbers as bfdb's card (`bfdb/src/range/forms.rs`).
 */
export const LENS_CELLS = 12
export const LENS_DEG_PER_CELL = 1.6 / LENS_CELLS

/** Where the ball is, in cells from the top (6 = level with the datum), and whether it is off the lens. */
export function ballPos(gse: number): { p: number; off: boolean } {
  const p = LENS_CELLS / 2 - gse / LENS_DEG_PER_CELL
  return { p, off: p < 0.25 || p > LENS_CELLS - 0.25 }
}

/** The one or two cells the ball lights: those whose middles are nearest it. */
export function litCells(p: number): number[] {
  const out: number[] = []
  for (let i = 0; i < LENS_CELLS; i++) if (Math.abs(p - (i + 0.5)) < 0.75) out.push(i)
  return out
}

/** The ball in words: "centred", "1.5 cells high", "off the bottom". */
export function ballSay(gse: number): string {
  const { p, off } = ballPos(gse)
  if (off) return gse > 0 ? 'off the top' : 'off the bottom'
  const c = Math.round((LENS_CELLS / 2 - p) * 2) / 2
  if (Math.abs(c) < 0.5) return 'centred'
  const red = litCells(p).some(i => i >= LENS_CELLS - 2)
  const a = Math.abs(c)
  return `${a} cell${a === 1 ? '' : 's'} ${c > 0 ? 'high' : 'low'}${red ? ', red' : ''}`
}
