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
  url, corners, naturalW, naturalH, opacity, interactive, onClick,
}: {
  url: string
  corners: LatLon[]
  naturalW: number
  naturalH: number
  opacity: number
  interactive: boolean
  onClick?: () => void
}) {
  const map = useMap()
  const imgRef = useRef<HTMLImageElement | null>(null)
  const clickRef = useRef<(() => void) | undefined>(onClick)
  useEffect(() => { clickRef.current = onClick }, [onClick])

  // Create / destroy the <img> element.
  useEffect(() => {
    const pane = map.getPane('overlayPane')
    if (!pane) return
    const img = L.DomUtil.create('img', 'leaflet-zoom-hide') as HTMLImageElement
    img.src = url
    img.alt = ''
    img.decoding = 'async'
    img.draggable = false
    Object.assign(img.style, {
      position: 'absolute', left: '0', top: '0', transformOrigin: '0 0',
      width: `${naturalW}px`, height: `${naturalH}px`,
    })
    const onImgClick = (e: MouseEvent) => { L.DomEvent.stop(e); clickRef.current?.() }
    img.addEventListener('click', onImgClick)
    pane.appendChild(img)
    imgRef.current = img
    return () => {
      img.removeEventListener('click', onImgClick)
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
  }, [opacity, interactive])

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
