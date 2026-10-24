import { useEffect, useMemo, useState, type CSSProperties } from 'react'
import { campaign } from '../config/campaign'
import { useTheme } from '../context/ThemeContext'

// ─────────────────────────────────────────────────────────────────────────────
// Wireframe art behind a panel: the top bar's carrier, the login page's
// Flanker, a jet, helicopter or tank on each page header. The line work is
// the flight replay's real 3D models (public/models, credits in its
// CREDITS.txt) cut down to their creases and silhouettes by
// scripts/gen-wireframes.py, with hidden-line removal: lines the camera can
// see are drawn strong, the ones behind the surface faint. This paints it in
// the campaign's accent colour, faded out toward the panel's middle so text
// over it stays readable.
//
// Each view is a JSON file under /media/wire, fetched the first time a page
// shows it and kept for the session: the art is 50-150 KB a view, which has
// no business in the main bundle.
//
// Like the flags it replaces, it is a CSS background layer, not an element:
// the top bar and the KPI strip are plain flex/grid rows, and an overlay
// would paint over their unpositioned content or trap their dropdowns.
// ─────────────────────────────────────────────────────────────────────────────

export type WireframeName =
  | 'carrier' | 'carrier-quarter' | 'flanker' | 'flanker-rear'
  | 'hornet' | 'hornet-top' | 'apache' | 'abrams'

interface WireframeArt {
  w: number
  h: number
  /** Visible near, visible far, hidden. */
  paths: [string, string, string]
}

/** How strongly the lines show. Busy, text-dense rows want less. */
export type WireStrength = 'faint' | 'soft' | 'bold'

// The models are dense (thousands of edges): at the strength a hand-drawn
// outline wants they read as a picture pasted on the panel, not line work
// sunk into it.
const ALPHA: Record<WireStrength, { dark: number; light: number }> = {
  faint: { dark: 0.26, light: 0.24 },
  soft: { dark: 0.38, light: 0.34 },
  bold: { dark: 0.55, light: 0.48 },
}
/** Visible near, visible far, hidden. */
const LAYER = [1, 0.62, 0.16]

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

const loaded = new Map<WireframeName, WireframeArt>()
const loading = new Map<WireframeName, Promise<WireframeArt | null>>()

function load(name: WireframeName): Promise<WireframeArt | null> {
  let p = loading.get(name)
  if (!p) {
    p = fetch(`${import.meta.env.BASE_URL}media/wire/${name}.json`)
      .then(r => (r.ok ? (r.json() as Promise<WireframeArt>) : null))
      .then(a => { if (a) loaded.set(name, a); return a })
      // Decoration: a failed fetch just leaves the panel plain.
      .catch(() => null)
    loading.set(name, p)
  }
  return p
}

function useArt(name: WireframeName): WireframeArt | undefined {
  // Read from the cache on every render; the state only exists to render
  // again once a fetch lands.
  const [, setTick] = useState(0)
  useEffect(() => {
    if (loaded.has(name)) return
    let live = true
    load(name).then(a => { if (live && a) setTick(t => t + 1) })
    return () => { live = false }
  }, [name])
  const art = loaded.get(name)
  return art?.paths ? art : undefined
}

function svg(a: WireframeArt, color: string, alpha: number, fade: WireOptions['fade']): string {
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
    .map((d, i) => d && `<path d="${d}" fill="none" stroke="${paint}" stroke-opacity="${(alpha * LAYER[i]).toFixed(3)}" stroke-width="1" stroke-linejoin="round" vector-effect="non-scaling-stroke"/>`)
    .join('')
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${a.w} ${a.h}">${defs}${paths}</svg>`
}

/** `background` for a panel that carries a wireframe (just `base` until it has loaded). */
export function useWireframe(name: WireframeName, opts: WireOptions = {}): CSSProperties {
  const { theme } = useTheme()
  const art = useArt(name)
  const { base = 'transparent', strength = 'soft', position = 'right center', size = 'auto 100%', fade = 'left' } = opts
  const color = campaign.accentColor
  return useMemo(() => {
    if (!art) return { background: base }
    const url = `url("data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg(art, color, ALPHA[strength][theme], fade))}")`
    return { background: `${url} ${position} / ${size} no-repeat, ${base}` }
  }, [art, color, strength, theme, fade, position, size, base])
}
