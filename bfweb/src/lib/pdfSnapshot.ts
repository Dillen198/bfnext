// A page section saved as a one-page PDF, looking as it does on screen.
//
// html-to-image turns the element into an SVG (a foreignObject with every
// computed style, the web fonts and the background images inlined), which the
// browser draws with its own renderer, so the dashboard's CSS and the
// recharts SVGs come out as they are on screen. The PDF page is sized to the
// snapshot, not to A4, so the whole thing is always exactly one page.
//
// We load and draw the SVG ourselves rather than calling toCanvas: that
// waits on requestAnimationFrame with no timeout, so an export started in a
// background tab hung until the tab was looked at. Here it fails instead.

import { toSvg } from 'html-to-image'
import { jsPDF } from 'jspdf'

/** Elements with this attribute (buttons, mostly) are left out of the PDF. */
export const NO_PDF = 'data-no-pdf'

/** The colour behind an element: the first ancestor that paints one. */
function backdrop(el: HTMLElement): string {
  for (let e: HTMLElement | null = el; e; e = e.parentElement) {
    const c = getComputedStyle(e).backgroundColor
    if (c && c !== 'transparent' && !/^rgba\(.*,\s*0\)$/.test(c)) return c
  }
  return '#0b0b0b'
}

/** The next painted frame, or `ms` if the tab isn't painting. */
const frame = (ms = 1000) => new Promise<void>(r => {
  const t = setTimeout(r, ms)
  requestAnimationFrame(() => { clearTimeout(t); r() })
})

/**
 * The SVG as an image, ready to draw. Chrome rasterises a foreignObject
 * image when it paints a frame, so we wait for one after decoding.
 */
async function load(src: string): Promise<HTMLImageElement> {
  const img = new Image()
  await new Promise<void>((resolve, reject) => {
    img.onload = () => resolve()
    img.onerror = () => reject(new Error('the snapshot did not load as an image'))
    img.src = src
  })
  await img.decode().catch(() => undefined)
  await frame()
  return img
}

/** True if nothing was drawn: every sampled pixel still transparent. */
function blank(ctx: CanvasRenderingContext2D, w: number, h: number): boolean {
  const d = ctx.getImageData(0, 0, w, h).data
  for (let i = 3; i < d.length; i += 4 * 97) if (d[i] !== 0) return false
  return true
}

const skip = (n: Node) => !(n instanceof Element && n.hasAttribute(NO_PDF))

/** Let fade-ins finish: the snapshot takes each element's style as it is now.
 *  (Not the endless ones, like the LIVE dot's pulse.) */
async function settle(el: HTMLElement) {
  await Promise.all(el.getAnimations({ subtree: true })
    .filter(a => a.effect?.getTiming().iterations !== Infinity)
    .map(a => a.finished.catch(() => undefined)))
}

/** How far the widest sideways-scrolling box inside `el` is cut off, in px. */
function cutOff(el: HTMLElement): number {
  let worst = 0
  for (const e of el.querySelectorAll<HTMLElement>('*')) {
    const o = getComputedStyle(e).overflowX
    if ((o === 'auto' || o === 'scroll') && e.scrollWidth > e.clientWidth + 1) worst = Math.max(worst, e.scrollWidth - e.clientWidth)
  }
  return worst
}

/**
 * A PDF can't scroll, so lay `el` out wide enough that nothing in it does
 * (the pilots table, on a narrow screen). It grows by twice the shortfall a
 * step, since a two-column grid gives each column half, and waits for the
 * charts to re-fit. Returns how to put it back.
 */
async function widenToFit(el: HTMLElement, max = 1800): Promise<() => void> {
  const before = el.style.width
  const restore = () => { el.style.width = before }
  for (let step = 0; step < 4; step++) {
    const short = cutOff(el)
    if (short === 0 || el.offsetWidth >= max) break
    el.style.width = `${Math.min(max, el.offsetWidth + short * 2 + 8)}px`
    await frame()
    await frame()
  }
  return restore
}

/**
 * `node` drawn `width` x `height` CSS px on a canvas at `scale`, with `style`
 * applied to its root. Drawn on a clear canvas first: if the browser hasn't
 * rasterised the snapshot (a tab in the background), say so rather than
 * save a blank page.
 */
async function snap(node: HTMLElement, width: number, height: number, scale: number, style: Partial<CSSStyleDeclaration>) {
  const img = await load(await toSvg(node, { width, height, style, filter: skip }))
  const c = document.createElement('canvas')
  c.width = Math.round(width * scale)
  c.height = Math.round(height * scale)
  const ctx = c.getContext('2d')
  if (!ctx) throw new Error('no 2D canvas')
  ctx.drawImage(img, 0, 0, c.width, c.height)
  for (let tries = 0; blank(ctx, c.width, c.height); tries++) {
    if (tries === 3) throw new Error('the snapshot came out blank; keep the tab in view and try again')
    await frame(1500)
    ctx.drawImage(img, 0, 0, c.width, c.height)
  }
  return c
}

/**
 * Save `el` as `filename`: one page, in the colours it has on screen, with
 * `margin` CSS pixels of its backdrop around it. The margin is part of the
 * snapshot, so things that stick out of the element's box (the tape on a
 * Levant card) are kept. `header`, if given (the page's own header band, with
 * its art), runs across the top of the page edge to edge, as it does on screen.
 */
export async function saveElementPdf(el: HTMLElement, filename: string, { header, margin = 24, scale = 2 }: {
  header?: HTMLElement | null; margin?: number; scale?: number
} = {}) {
  await settle(el)
  if (header) await settle(header)

  const restore = await widenToFit(el)
  let body: HTMLCanvasElement
  let head: HTMLCanvasElement | null
  let width: number
  let height: number
  try {
    width = el.offsetWidth + margin * 2
    const bodyH = el.offsetHeight + margin * 2
    const headH = header ? header.offsetHeight : 0
    height = headH + bodyH
    body = await snap(el, width, bodyH, scale,
      { boxSizing: 'content-box', width: `${el.offsetWidth}px`, padding: `${margin}px`, margin: '0' })
    head = header
      ? await snap(header, width, headH, scale,
          { boxSizing: 'border-box', width: `${width}px`, margin: '0', position: 'relative', left: '0', top: '0' })
      : null
  } finally {
    restore()
  }
  const bg = backdrop(el)

  const page = document.createElement('canvas')
  page.width = Math.round(width * scale)
  page.height = Math.round(height * scale)
  const ctx = page.getContext('2d')
  if (!ctx) throw new Error('no 2D canvas')
  ctx.fillStyle = bg
  ctx.fillRect(0, 0, page.width, page.height)
  if (head) ctx.drawImage(head, 0, 0)
  ctx.drawImage(body, 0, head ? head.height : 0)

  // CSS px to PDF points (96 to 72 per inch). JPEG, because jsPDF embeds
  // one as it is; a PNG it decodes and recompresses in JS, which takes many
  // seconds at this size.
  const w = width * 0.75
  const h = height * 0.75
  const pdf = new jsPDF({ orientation: w > h ? 'landscape' : 'portrait', unit: 'pt', format: [w, h] })
  pdf.addImage(page.toDataURL('image/jpeg', 0.92), 'JPEG', 0, 0, w, h)
  pdf.save(filename)
}
