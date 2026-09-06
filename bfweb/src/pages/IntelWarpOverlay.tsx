import { useEffect, useRef } from 'react'
import { useMap } from 'react-leaflet'
import L from 'leaflet'
import { matrix3dForQuad } from '../lib/warp'
import type { LatLon } from '../lib/geo'

const MAX_CANVAS_DIM = 1600
const TARGET_LUMA = 150

/** Load `url` and normalise the frame so a run of overlapping warped photos
 *  reads as one mosaic instead of a stack of hard-edged, unevenly-exposed
 *  wedges:
 *   - drop the near-black matte (DCS TARPS shots are letterboxed on black),
 *   - feather the frame edges so neighbours cross-fade,
 *   - pull each frame's mean brightness toward a common target so a dark
 *     capture doesn't sit on the map as a grey box.
 *  Returns an object URL for the processed PNG; falls back to the original
 *  url if the canvas is tainted or anything throws. */
function cleanPhoto(url: string): Promise<{ href: string; revoke: boolean }> {
  return new Promise(resolve => {
    const im = new Image()
    im.crossOrigin = 'use-credentials'
    im.onload = () => {
      try {
        const scale = Math.min(1, MAX_CANVAS_DIM / Math.max(im.naturalWidth, im.naturalHeight))
        const w = Math.max(1, Math.round(im.naturalWidth * scale))
        const h = Math.max(1, Math.round(im.naturalHeight * scale))
        const cv = document.createElement('canvas')
        cv.width = w; cv.height = h
        const ctx = cv.getContext('2d')
        if (!ctx) return resolve({ href: url, revoke: false })
        ctx.drawImage(im, 0, 0, w, h)
        const data = ctx.getImageData(0, 0, w, h)
        const px = data.data
        const feather = Math.max(2, Math.round(Math.min(w, h) * 0.08))

        // pass 1 — alpha (matte + edge feather) and mean luminance of the kept pixels
        let lumaSum = 0, lumaN = 0
        for (let y = 0; y < h; y++) {
          const ey = y < h - 1 - y ? y : h - 1 - y
          for (let x = 0; x < w; x++) {
            const i = (y * w + x) * 4
            const r = px[i], g = px[i + 1], b = px[i + 2]
            const m = r > g ? (r > b ? r : b) : (g > b ? g : b)
            if (m <= 14) { px[i + 3] = 0; continue }
            let a = px[i + 3]
            if (m < 44) a = (a * (m - 14)) / 30
            const e = x < ey ? (x < w - 1 - x ? x : w - 1 - x) : ey
            if (e < feather) a = (a * e) / feather
            px[i + 3] = a < 0 ? 0 : (a > 255 ? 255 : Math.round(a))
            if (a > 24) { lumaSum += 0.299 * r + 0.587 * g + 0.114 * b; lumaN++ }
          }
        }

        // pass 2 — exposure match
        const mean = lumaN ? lumaSum / lumaN : TARGET_LUMA
        let gain = TARGET_LUMA / Math.max(1, mean)
        gain = gain < 0.65 ? 0.65 : gain > 1.7 ? 1.7 : gain
        if (Math.abs(gain - 1) > 0.03) {
          for (let i = 0; i < px.length; i += 4) {
            if (px[i + 3] === 0) continue
            px[i] = Math.min(255, px[i] * gain)
            px[i + 1] = Math.min(255, px[i + 1] * gain)
            px[i + 2] = Math.min(255, px[i + 2] * gain)
          }
        }

        ctx.putImageData(data, 0, 0)
        cv.toBlob(b => {
          if (b) resolve({ href: URL.createObjectURL(b), revoke: true })
          else resolve({ href: url, revoke: false })
        }, 'image/png')
      } catch {
        resolve({ href: url, revoke: false })
      }
    }
    im.onerror = () => resolve({ href: url, revoke: false })
    im.src = url
  })
}

/** Renders one recon photo perspective-warped onto a ground quad, as an
 *  `<img>` in Leaflet's overlay pane transformed with `matrix3d`. The quad
 *  corners are [TL, TR, BR, BL]; the image is hidden while its quad is
 *  degenerate (e.g. edge-on to the camera). */
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
  const imgRef = useRef<HTMLImageElement | null>(null)
  const clickRef = useRef<(() => void) | undefined>(onClick)
  const ctxRef = useRef<(() => void) | undefined>(onContextMenu)
  const opacityRef = useRef(opacity)
  useEffect(() => { opacityRef.current = opacity }, [opacity])
  useEffect(() => { clickRef.current = onClick }, [onClick])
  useEffect(() => { ctxRef.current = onContextMenu }, [onContextMenu])

  // Create / destroy the <img> element.
  useEffect(() => {
    const pane = map.getPane('overlayPane')
    if (!pane) return
    const img = L.DomUtil.create('img', 'leaflet-zoom-hide') as HTMLImageElement
    // Photos are served cross-origin from bfdb and gated on the session
    // cookie, so the request has to carry credentials.
    img.crossOrigin = 'use-credentials'
    img.alt = ''
    img.decoding = 'async'
    img.draggable = false
    Object.assign(img.style, {
      position: 'absolute', left: '0', top: '0', transformOrigin: '0 0',
      width: `${naturalW}px`, height: `${naturalH}px`,
      // Tailwind's preflight (`img{max-width:100%;height:auto}`) otherwise
      // clamps the image to the (near-zero-width) overlay pane and collapses
      // the warp — same fix MapPage uses for its sprite icons.
      maxWidth: 'none', maxHeight: 'none',
    })
    const onImgClick = (e: MouseEvent) => { L.DomEvent.stop(e); clickRef.current?.() }
    const onImgCtx = (e: MouseEvent) => { L.DomEvent.stop(e); e.preventDefault(); ctxRef.current?.() }
    img.addEventListener('click', onImgClick)
    img.addEventListener('contextmenu', onImgCtx)
    // Stay invisible until the bitmap is actually decoded — otherwise a
    // sized-but-src-less <img> flashes as a box while the (async) fetch +
    // canvas pass runs, and with dozens of captures loading at once that's
    // a screenful of dark rectangles.
    img.style.opacity = '0'
    img.style.transition = 'opacity 160ms linear'
    const reveal = () => { img.dataset.ready = '1'; img.style.opacity = String(opacityRef.current) }
    img.addEventListener('load', reveal)
    pane.appendChild(img)
    imgRef.current = img

    let cancelled = false
    let objectUrl: string | null = null
    if (keyBlack) {
      cleanPhoto(url).then(({ href, revoke }) => {
        if (cancelled) { if (revoke) URL.revokeObjectURL(href); return }
        objectUrl = revoke ? href : null
        img.src = href
      })
    } else {
      img.src = url
    }

    return () => {
      cancelled = true
      img.removeEventListener('click', onImgClick)
      img.removeEventListener('contextmenu', onImgCtx)
      img.removeEventListener('load', reveal)
      img.remove()
      imgRef.current = null
      if (objectUrl) URL.revokeObjectURL(objectUrl)
    }
  }, [map, url, naturalW, naturalH, keyBlack])

  // Cheap style updates.
  useEffect(() => {
    const img = imgRef.current
    if (!img) return
    if (img.dataset.ready === '1') img.style.opacity = String(opacity)
    img.style.pointerEvents = interactive ? 'auto' : 'none'
    img.style.cursor = interactive ? 'pointer' : 'default'
    if (zIndex != null) img.style.zIndex = String(zIndex)
  }, [opacity, interactive, zIndex])

  // Reproject on view changes.
  useEffect(() => {
    const img = imgRef.current
    if (!img) return
    const update = () => {
      const pts = corners.map(c => {
        const p = map.latLngToLayerPoint(L.latLng(c[0], c[1]))
        return { x: p.x, y: p.y }
      })
      const t = matrix3dForQuad(pts, naturalW, naturalH)
      if (t) { img.style.transform = t; img.style.display = '' }
      else { img.style.display = 'none' }
    }
    update()
    map.on('zoomend viewreset moveend', update)
    return () => { map.off('zoomend viewreset moveend', update) }
  }, [map, corners, naturalW, naturalH])

  return null
}
