import type { CSSProperties } from 'react'
import { useInstance } from '../context/InstanceContext'

// ─────────────────────────────────────────────────────────────────────────────
// Each server's two flags, blended into a panel's background: the blue side's
// from the left edge, the red side's from the right. The public site's
// /servers cards do the same.
//
// The flags are CSS background layers, not elements. The topbar and the KPI
// strip are plain flex/grid rows; an absolutely positioned overlay would
// paint over their unpositioned content, or need every child repositioned
// and given its own stacking context, which traps dropdowns. A background
// cannot do either. The PNGs (public/media/flag-*.png) already fade toward
// the middle, so a layer is just "picture at the edge". A veil of the
// panel's own colour on top sets how strong they read, per theme.
// ─────────────────────────────────────────────────────────────────────────────

interface FlagPair {
  left: string
  right: string
  /** Mostly white country flags read far brighter than the coloured cloth ones. */
  bright?: boolean
}

const COALITION: FlagPair = { left: '/media/flag-blufor-l.png', right: '/media/flag-redfor-r.png' }

/** Flags per bfdb instance id. A server not listed flies the coalition colours. */
const BY_INSTANCE: Record<string, FlagPair> = {
  // NATO against Russia, Syria, Iran and Iraq; scripts/gen-flags.py draws both.
  vs1: { left: '/media/flag-nato-l.png', right: '/media/flag-rsii-r.png' },
  vs2: { left: '/media/flag-georgia-l.png', right: '/media/flag-russia-r.png', bright: true },
}

/**
 * How much of the panel colour is laid over the flags, as a percentage.
 * Higher is fainter. Busy, text-dense rows get more veil.
 */
export type FlagStrength = 'faint' | 'soft' | 'bold'
const VEIL: Record<FlagStrength, number> = { faint: 88, soft: 80, bold: 68 }
const BRIGHT_EXTRA = 6

/**
 * `background` for a panel that should carry the current server's flags.
 * `base` is the panel's own background, which goes underneath unchanged (it
 * may be translucent, over the animated backdrop). `solid` is its opaque
 * twin: the veil is mixed from that, since a veil of a translucent colour
 * barely dims anything.
 */
export function useFlagBackground(
  base: string,
  solid: string,
  strength: FlagStrength = 'soft',
  /** Flag height as a share of the panel's; a full page wants them smaller than a strip does. */
  height = '100%',
): CSSProperties {
  const { current } = useInstance()
  const flags = (current && BY_INSTANCE[current.id]) || COALITION
  const pct = Math.min(96, VEIL[strength] + (flags.bright ? BRIGHT_EXTRA : 0))
  const veil = `color-mix(in srgb, ${solid} ${pct}%, transparent)`
  return {
    background: [
      `linear-gradient(${veil}, ${veil})`,
      `url(${flags.left}) left center / auto ${height} no-repeat`,
      `url(${flags.right}) right center / auto ${height} no-repeat`,
      base,
    ].join(', '),
  }
}
