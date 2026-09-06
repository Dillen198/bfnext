import { useEffect, useRef } from 'react'
import { useMap } from 'react-leaflet'
import L from 'leaflet'
import { matrix3dForQuad } from '../lib/warp'
import type { LatLon } from '../lib/geo'

/** Renders one recon photo perspective-warped onto a ground quad, as an
 *  `<img>` in Leaflet's overlay pane transformed with `matrix3d`. The quad
 *  corners are [TL, TR, BR, BL]; the image is hidden while its quad is
 *  degenerate (e.g. edge-on to the camera). */
export default function IntelWarpOverlay({
  url, corners, naturalW, naturalH, opacity, interactive, zIndex, onClick, onContextMenu,
}: {
  url: string
  corners: LatLon[]
  naturalW: number
  naturalH: number
  opacity: number
  interactive: boolean
  zIndex?: number
  onClick?: () => void
  onContextMenu?: () => void
}) {
  const map = useMap()
  const imgRef = useRef<HTMLImageElement | null>(null)
  const clickRef = useRef<(() => void) | undefined>(onClick)
  const ctxRef = useRef<(() => void) | undefined>(onContextMenu)
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
    img.src = url
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
    pane.appendChild(img)
    imgRef.current = img
    return () => {
      img.removeEventListener('click', onImgClick)
      img.removeEventListener('contextmenu', onImgCtx)
      img.remove()
      imgRef.current = null
    }
  }, [map, url, naturalW, naturalH])

  // Cheap style updates.
  useEffect(() => {
    const img = imgRef.current
    if (!img) return
    img.style.opacity = String(opacity)
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
