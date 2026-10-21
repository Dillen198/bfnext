import { useMemo, type CSSProperties } from 'react'
import { campaign } from '../config/campaign'
import { useTheme } from '../context/ThemeContext'
import { WIREFRAMES, type WireframeName } from './wireframes.gen'

// ─────────────────────────────────────────────────────────────────────────────
// Wireframe art behind a panel: the top bar's carrier, the login page's Su-27,
// a jet, helicopter or tank on each page header. The line work comes from
// scripts/gen-wireframes.py (3D models projected to SVG); this paints it in
// the campaign's accent colour, far lines fainter than near ones, faded out
// toward the panel's middle so text over it stays readable.
//
// Like the flags it replaces, it is a CSS background layer, not an element:
// the top bar and the KPI strip are plain flex/grid rows, and an overlay
// would paint over their unpositioned content or trap their dropdowns.
// ─────────────────────────────────────────────────────────────────────────────

export type { WireframeName }

/** How strongly the lines show. Busy, text-dense rows want less. */
export type WireStrength = 'faint' | 'soft' | 'bold'

const ALPHA: Record<WireStrength, { dark: number; light: number }> = {
  faint: { dark: 0.45, light: 0.4 },
  soft: { dark: 0.65, light: 0.55 },
  bold: { dark: 0.85, light: 0.7 },
}
/** Near, middle and far lines. */
const DEPTH = [1, 0.6, 0.34]

export interface WireOptions {
  /** The panel's own background, which goes underneath unchanged. */
  base?: string
  strength?: WireStrength
  /** CSS background-position. */
  position?: string
  /** CSS background-size. */
  size?: string
  /** Which edge the art fades out toward; 'none' for art on its own. */
  fade?: 'left' | 'right' | 'none'
}

function svg(name: WireframeName, color: string, alpha: number, fade: WireOptions['fade']): string {
  const a = WIREFRAMES[name]
  let defs = ''
  let paint = color
  if (fade && fade !== 'none') {
    // In user space, so every path fades across the same stretch of picture.
    const [x1, x2] = fade === 'left' ? [0, a.w * 0.55] : [a.w, a.w * 0.45]
    defs = `<defs><linearGradient id="f" gradientUnits="userSpaceOnUse" x1="${x1}" x2="${x2}" y1="0" y2="0">`
      + `<stop offset="0" stop-color="${color}" stop-opacity="0"/><stop offset="1" stop-color="${color}"/></linearGradient></defs>`
    paint = 'url(#f)'
  }
  const paths = a.paths
    .map((d, i) => d && `<path d="${d}" fill="none" stroke="${paint}" stroke-opacity="${(alpha * DEPTH[i]).toFixed(3)}" stroke-width="1.15" stroke-linejoin="round" vector-effect="non-scaling-stroke"/>`)
    .join('')
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${a.w} ${a.h}">${defs}${paths}</svg>`
}

/** `background` for a panel that carries a wireframe. */
export function useWireframe(name: WireframeName, opts: WireOptions = {}): CSSProperties {
  const { theme } = useTheme()
  const { base = 'transparent', strength = 'soft', position = 'right center', size = 'auto 100%', fade = 'left' } = opts
  const color = campaign.accentColor
  return useMemo(() => {
    const url = `url("data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg(name, color, ALPHA[strength][theme], fade))}")`
    return { background: `${url} ${position} / ${size} no-repeat, ${base}` }
  }, [name, color, strength, theme, fade, position, size, base])
}
