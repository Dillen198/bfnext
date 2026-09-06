import { useEffect, useRef } from 'react'
import { useMap } from 'react-leaflet'
import L from 'leaflet'
import { matrix3dForQuad } from '../lib/warp'
import type { LatLon } from '../lib/geo'

// Feather mask — a single radial gradient that fades every edge/corner so
// overlapping frames cross-blend instead of showing hard seams. Cheap: it's
// one GPU-composited property, no per-pixel work.
const FEATHER_MASK =
  'radial-gradient(ellipse 150% 150% at 50% 50%, #000 58%, rgba(0,0,0,0) 94%)'

/** The photos live in their own pane so `mix-blend-mode` blends them against
 *  each other (and a transparent backdrop) rather than the map tiles, and the
 *  whole mosaic then composites onto the map normally. */
function photoPane(map: L.Map): HTMLElement {
  let p = map.getPane('intel-photos')
  if (!p) {
    p = map.createPane('intel-photos')
    p.style.zIndex = '450'
    p.style.isolation = 'isolate'
    p.style.pointerEvents = 'none'
  }
  return p
}

/** Renders one recon photo perspective-warped onto a ground quad, as an
 *  `<img>` transformed with `matrix3d`. The image is hidden while its quad is
 *  degenerate (e.g. edge-on to the camera) and until its bitmap has decoded.
 *  With `blend` on (the default) the frame is edge-feathered and drawn with
 *  `mix-blend-mode: lighten`, so a run of overlapping captures reads as one
 *  mosaic — the black TARPS matte and darker frames drop out because a
 *  brighter neighbour always wins. */
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
  keyBlack?: boolean   // "blend": feather + lighten so overlaps mosaic cleanly
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
    const pane = photoPane(map)
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
      // clamps the image to the (near-zero-width) pane and collapses the warp
      // — same fix MapPage uses for its sprite icons.
      maxWidth: 'none', maxHeight: 'none',
      // Hidden until the bitmap decodes — a sized <img> with no pixels yet
      // flashes as a box, and with dozens of captures that's a screenful of
      // rectangles during load.
      opacity: '0',
      transition: 'opacity 160ms linear',
    })
    if (keyBlack) {
      img.style.mixBlendMode = 'lighten'
      img.style.webkitMaskImage = FEATHER_MASK
      img.style.maskImage = FEATHER_MASK
    }
    const onImgClick = (e: MouseEvent) => { L.DomEvent.stop(e); clickRef.current?.() }
    const onImgCtx = (e: MouseEvent) => { L.DomEvent.stop(e); e.preventDefault(); ctxRef.current?.() }
    const reveal = () => { img.dataset.ready = '1'; img.style.opacity = String(opacityRef.current) }
    img.addEventListener('click', onImgClick)
    img.addEventListener('contextmenu', onImgCtx)
    img.addEventListener('load', reveal)
    pane.appendChild(img)
    imgRef.current = img
    img.src = url

    return () => {
      img.removeEventListener('click', onImgClick)
      img.removeEventListener('contextmenu', onImgCtx)
      img.removeEventListener('load', reveal)
      img.remove()
      imgRef.current = null
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
