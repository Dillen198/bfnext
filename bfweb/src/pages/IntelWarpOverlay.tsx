import { useEffect, useRef } from 'react'
import { useMap } from 'react-leaflet'
import L from 'leaflet'
import { matrix3dForQuad } from '../lib/warp'
import type { LatLon } from '../lib/geo'

// Only downscale genuinely huge frames — keep native resolution otherwise so
// the imagery stays sharp when you zoom in. A 2560px cap is ~15 MB of canvas
// per frame; with a busy run that's the ceiling worth paying.
const MAX_DIM = 2560

/** Draw `src` into a canvas (long edge capped at MAX_DIM), knock the near-black
 *  TARPS matte out to transparent and feather the frame edges so overlapping
 *  frames cross-fade. Resolves the prepared canvas, or null if the pixels
 *  can't be read (tainted) or the load fails. */
function prepareCanvas(src: string): Promise<HTMLCanvasElement | null> {
  return new Promise(resolve => {
    const im = new Image()
    im.crossOrigin = 'use-credentials'
    im.decoding = 'async'
    im.onerror = () => resolve(null)
    im.onload = () => {
      try {
        const nw = im.naturalWidth || 1, nh = im.naturalHeight || 1
        const scale = Math.min(1, MAX_DIM / Math.max(nw, nh))
        const w = Math.max(1, Math.round(nw * scale))
        const h = Math.max(1, Math.round(nh * scale))
        const cv = document.createElement('canvas')
        cv.width = w; cv.height = h
        const ctx = cv.getContext('2d')
        if (!ctx) return resolve(null)
        ctx.imageSmoothingEnabled = true
        ctx.imageSmoothingQuality = 'high'
        ctx.drawImage(im, 0, 0, w, h)
        const data = ctx.getImageData(0, 0, w, h)   // throws if tainted
        const px = data.data
        const feather = Math.max(1, Math.round(Math.min(w, h) * 0.035))
        for (let y = 0; y < h; y++) {
          const ey = y < h - 1 - y ? y : h - 1 - y
          for (let x = 0; x < w; x++) {
            const i = (y * w + x) * 4
            const r = px[i], g = px[i + 1], b = px[i + 2]
            const mx = r > g ? (r > b ? r : b) : (g > b ? g : b)
            if (mx <= 16) { px[i + 3] = 0; continue }
            let a = px[i + 3]
            if (mx < 46) a = (a * (mx - 16)) / 30            // fade the matte edge
            const e = x < ey ? (x < w - 1 - x ? x : w - 1 - x) : ey
            if (e < feather) a = (a * e) / feather           // feather frame border
            px[i + 3] = a < 0 ? 0 : a > 255 ? 255 : a
          }
        }
        ctx.putImageData(data, 0, 0)
        resolve(cv)
      } catch {
        resolve(null)
      }
    }
    im.src = src
  })
}

/** Renders one recon photo perspective-warped onto a ground quad. The photo
 *  is prepared once (matte knocked out, edges feathered) into a `<canvas>`
 *  which is then CSS `matrix3d`-warped onto the quad — falling back to a
 *  plain `<img>` if the pixels can't be read. Hidden while the quad is
 *  degenerate or the bitmap isn't ready. */
export default function IntelWarpOverlay({
  url, corners, naturalW, naturalH, opacity, interactive, zIndex, keyBlack = true, onClick, onContextMenu,
}: {
  url: string
  corners: LatLon[]
  naturalW: number
  naturalH: number
  opacity: number
  interactive: boolean
  zIndex?: number
  keyBlack?: boolean
  onClick?: () => void
  onContextMenu?: () => void
}) {
  const map = useMap()
  const elRef = useRef<HTMLElement | null>(null)
  const sizeRef = useRef({ w: naturalW, h: naturalH })
  const cornersRef = useRef(corners)
  const clickRef = useRef<(() => void) | undefined>(onClick)
  const ctxRef = useRef<(() => void) | undefined>(onContextMenu)
  const opacityRef = useRef(opacity)
  useEffect(() => { opacityRef.current = opacity }, [opacity])
  useEffect(() => { clickRef.current = onClick }, [onClick])
  useEffect(() => { ctxRef.current = onContextMenu }, [onContextMenu])

  const reproject = () => {
    const node = elRef.current
    if (!node) return
    const { w, h } = sizeRef.current
    const pts = cornersRef.current.map(c => {
      const p = map.latLngToLayerPoint(L.latLng(c[0], c[1]))
      return { x: p.x, y: p.y }
    })
    const t = matrix3dForQuad(pts, w, h)
    if (t) { node.style.transform = t; node.style.display = '' }
    else { node.style.display = 'none' }
  }

  // Create / destroy the overlay element.
  useEffect(() => {
    const pane = map.getPane('overlayPane')
    if (!pane) return
    let cancelled = false

    const mount = (node: HTMLElement, w: number, h: number) => {
      if (cancelled) { return }
      node.classList.add('leaflet-zoom-hide')
      Object.assign(node.style, {
        position: 'absolute', left: '0', top: '0', transformOrigin: '0 0',
        width: `${w}px`, height: `${h}px`, maxWidth: 'none', maxHeight: 'none',
        display: 'none', opacity: '0', transition: 'opacity 160ms linear',
      })
      const onC = (e: MouseEvent) => { L.DomEvent.stop(e); clickRef.current?.() }
      const onX = (e: MouseEvent) => { L.DomEvent.stop(e); e.preventDefault(); ctxRef.current?.() }
      node.addEventListener('click', onC)
      node.addEventListener('contextmenu', onX)
      cleanupListeners = () => {
        node.removeEventListener('click', onC)
        node.removeEventListener('contextmenu', onX)
      }
      pane.appendChild(node)
      elRef.current = node
      sizeRef.current = { w, h }
      node.dataset.ready = '1'
      reproject()
      node.style.opacity = String(opacityRef.current)
    }

    let cleanupListeners: (() => void) | null = null

    const loadImg = () => {
      const img = new Image()
      img.crossOrigin = 'use-credentials'
      img.draggable = false
      img.alt = ''
      img.onload = () => mount(img, naturalW, naturalH)
      img.onerror = () => mount(img, naturalW, naturalH)
      img.src = url
    }

    if (keyBlack) {
      prepareCanvas(url).then(cv => {
        if (cancelled) return
        if (cv) mount(cv, cv.width, cv.height)
        else loadImg()
      })
    } else {
      loadImg()
    }

    map.on('zoomend viewreset moveend', reproject)
    return () => {
      cancelled = true
      map.off('zoomend viewreset moveend', reproject)
      cleanupListeners?.()
      elRef.current?.remove()
      elRef.current = null
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [map, url, naturalW, naturalH, keyBlack])

  // Keep the projection current when the quad changes.
  useEffect(() => {
    cornersRef.current = corners
    reproject()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [corners])

  // Cheap style updates.
  useEffect(() => {
    const node = elRef.current
    if (!node) return
    if (node.dataset.ready === '1') node.style.opacity = String(opacity)
    node.style.pointerEvents = interactive ? 'auto' : 'none'
    node.style.cursor = interactive ? 'pointer' : 'default'
    if (zIndex != null) node.style.zIndex = String(zIndex)
  }, [opacity, interactive, zIndex])

  return null
}
