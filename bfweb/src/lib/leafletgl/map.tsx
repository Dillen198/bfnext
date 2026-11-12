// React components for the Leaflet stand-in (./shim): <LeafletGLMap> in
// place of react-map-gl's <Map>, and the Marker / Source / Layer that go in
// it. components/GLMap chooses between these and react-map-gl's.
import L from 'leaflet'
import {
  forwardRef, useContext, useEffect, useImperativeHandle, useLayoutEffect, useRef, useState,
  type CSSProperties, type ReactNode,
} from 'react'
import { createPortal } from 'react-dom'
import { DomMarker, ShimLayer, ShimMap, ShimSource, ZOOM_OFFSET, applyStyle, type LayerSpec } from './shim'
import { MapCtx, SourceCtx } from './context'

type Dict = Record<string, unknown>

type ViewState = { longitude?: number; latitude?: number; zoom?: number; bounds?: [[number, number], [number, number]]; fitBoundsOptions?: { padding?: number | Dict; maxZoom?: number } }

export interface LeafletGLMapProps {
  children?: ReactNode
  style?: CSSProperties
  className?: string
  mapStyle?: unknown
  initialViewState?: ViewState
  doubleClickZoom?: boolean
  scrollZoom?: boolean
  dragPan?: boolean
  minZoom?: number
  maxZoom?: number
  onLoad?: (e: Dict) => void
  onStyleData?: (e: Dict) => void
  onZoom?: (e: Dict) => void
  onMove?: (e: Dict) => void
  onMoveEnd?: (e: Dict) => void
  onZoomEnd?: (e: Dict) => void
  onClick?: (e: Dict) => void
  // Everything else react-map-gl takes (rotation, pitch, attribution ...) does not apply here.
}

export const LeafletGLMap = forwardRef<ShimMap | null, LeafletGLMapProps>(function LeafletGLMap(props, ref) {
  const holder = useRef<HTMLDivElement>(null)
  const [map, setMap] = useState<ShimMap | null>(null)
  const cb = useRef(props)
  useLayoutEffect(() => {
    cb.current = props
  })
  const tiles = useRef<L.TileLayer[]>([])
  const firstStyle = useRef(true)
  // The live Leaflet map. Taken down one tick after the effect's cleanup, and
  // only if the effect has not run again by then: React's development double
  // mount runs cleanup and effect back to back, and the children it already
  // rendered hold on to this map, so it must survive that.
  const live = useRef<{ shim: ShimMap; kill: number; destroy?: () => void } | null>(null)

  useLayoutEffect(() => {
    const scheduleDestroy = () => {
      const l = live.current
      if (!l) return
      l.kill = window.setTimeout(() => l.destroy?.(), 0)
    }
    if (live.current) {
      window.clearTimeout(live.current.kill)
      live.current.kill = 0
      return scheduleDestroy
    }
    const el = holder.current
    if (!el) return
    const p = cb.current
    const lmap = L.map(el, {
      zoomControl: false,
      attributionControl: false,
      zoomSnap: 0,
      zoomDelta: 0.5,
      wheelPxPerZoomLevel: 100,
      // Markers and the pages' canvases are placed by hand: no CSS zoom animation to keep up with.
      zoomAnimation: false,
      boxZoom: false,
      keyboard: false,
      doubleClickZoom: p.doubleClickZoom !== false,
      scrollWheelZoom: p.scrollZoom !== false,
      dragging: p.dragPan !== false,
      minZoom: p.minZoom != null ? p.minZoom + ZOOM_OFFSET : 1,
      maxZoom: p.maxZoom != null ? p.maxZoom + ZOOM_OFFSET : 20,
    })
    const v: ViewState = p.initialViewState ?? {}
    if (v.bounds) {
      const [[w, s], [e, n]] = v.bounds
      const pad = v.fitBoundsOptions?.padding
      const pp = typeof pad === 'number' ? { top: pad, bottom: pad, left: pad, right: pad } : ((pad ?? {}) as Record<string, number>)
      lmap.fitBounds(L.latLngBounds([s, w], [n, e]), {
        paddingTopLeft: [pp.left ?? 0, pp.top ?? 0],
        paddingBottomRight: [pp.right ?? 0, pp.bottom ?? 0],
        maxZoom: v.fitBoundsOptions?.maxZoom != null ? v.fitBoundsOptions.maxZoom + ZOOM_OFFSET : undefined,
        animate: false,
      })
    } else {
      lmap.setView([v.latitude ?? 0, v.longitude ?? 0], (v.zoom ?? 2) + ZOOM_OFFSET, { animate: false })
    }
    tiles.current = applyStyle(lmap, el, p.mapStyle, [])
    const shim = new ShimMap(lmap)
    live.current = { shim, kill: 0 }
    const viewState = () => ({ ...shim.getCenter(), longitude: shim.getCenter().lng, latitude: shim.getCenter().lat, zoom: shim.getZoom(), bearing: 0, pitch: 0 })
    shim.on('zoom', (e) => cb.current.onZoom?.({ ...e, viewState: viewState() }))
    shim.on('zoomend', (e) => cb.current.onZoomEnd?.({ ...e, viewState: viewState() }))
    shim.on('move', (e) => cb.current.onMove?.({ ...e, viewState: viewState() }))
    shim.on('moveend', (e) => cb.current.onMoveEnd?.({ ...e, viewState: viewState() }))
    shim.on('click', (e) => cb.current.onClick?.(e))
    setMap(shim)
    const ro = new ResizeObserver(() => lmap.invalidateSize({ pan: false }))
    ro.observe(el)
    // MapLibre's load comes after the children's sources and layers are in; so here.
    const t = window.setTimeout(() => {
      shim.fire('styledata', { dataType: 'style' })
      cb.current.onStyleData?.({ type: 'styledata', target: shim, dataType: 'style' })
      shim.fire('load')
      cb.current.onLoad?.({ type: 'load', target: shim })
      shim.triggerRepaint()
    }, 0)
    const destroy = () => {
      window.clearTimeout(t)
      ro.disconnect()
      shim.destroy()
      lmap.remove()
      live.current = null
    }
    live.current.destroy = destroy
    return scheduleDestroy
  }, [])

  // A new look (SAT / TOPO / TAC): new tiles, and the styledata MapLibre would send.
  useEffect(() => {
    if (!map || !holder.current) return
    if (firstStyle.current) {
      firstStyle.current = false
      return
    }
    tiles.current = applyStyle(map.lmap, holder.current, props.mapStyle, tiles.current)
    map.fire('styledata', { dataType: 'style' })
    cb.current.onStyleData?.({ type: 'styledata', target: map, dataType: 'style' })
  }, [map, props.mapStyle])

  useImperativeHandle(ref, () => map as ShimMap, [map])

  return (
    // isolation: Leaflet's panes have z-indexes of 200-800; kept inside the map,
    // so the page's own panels (z 1-10) stay over it, as they do over MapLibre's canvas.
    <div className={props.className} style={{ position: 'relative', width: '100%', height: '100%', isolation: 'isolate', ...props.style }}>
      <div ref={holder} className="lgl-map" style={{ position: 'absolute', inset: 0 }} />
      {map && <MapCtx.Provider value={map}>{props.children}</MapCtx.Provider>}
    </div>
  )
})

export function ShimMarker({ longitude, latitude, anchor = 'center', offset, children }: {
  longitude: number
  latitude: number
  anchor?: string
  offset?: [number, number] | { x: number; y: number }
  children?: ReactNode
}) {
  const map = useContext(MapCtx)
  const [el] = useState(() => document.createElement('div'))
  const mk = useRef<DomMarker | null>(null)
  const off: [number, number] | undefined = Array.isArray(offset) ? offset : offset ? [offset.x, offset.y] : undefined
  useLayoutEffect(() => {
    if (!map) return
    const m = new DomMarker(map, el, anchor, off)
    mk.current = m
    return () => {
      m.remove()
      mk.current = null
    }
    // the marker is made once; its anchor and place follow below
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [map, el])
  useLayoutEffect(() => {
    mk.current?.setAnchor(anchor, off).setLngLat([longitude, latitude])
  }, [longitude, latitude, anchor, off?.[0], off?.[1]]) // eslint-disable-line react-hooks/exhaustive-deps
  return createPortal(children, el)
}

export function ShimSourceEl({ id, data, children }: { id: string; data?: unknown; children?: ReactNode }) {
  const map = useContext(MapCtx)
  const [src] = useState(() => new ShimSource(id))
  useLayoutEffect(() => {
    if (!map) return
    map.sources.set(id, src)
    return () => {
      if (map.sources.get(id) === src) map.sources.delete(id)
    }
  }, [map, id, src])
  useLayoutEffect(() => {
    src.setData(data)
  }, [src, data])
  return <SourceCtx.Provider value={src}>{children}</SourceCtx.Provider>
}

export function ShimLayerEl(props: LayerSpec) {
  const map = useContext(MapCtx)
  const src = useContext(SourceCtx)
  const layer = useRef<ShimLayer | null>(null)
  const key = JSON.stringify([props.paint, props.layout, props.filter, props.minzoom, props.maxzoom])
  useLayoutEffect(() => {
    if (!map || !src) return
    const l = new ShimLayer(map, src, props.id, props.type, props)
    map.layers.set(props.id, l)
    layer.current = l
    return () => {
      l.remove()
      if (map.layers.get(props.id) === l) map.layers.delete(props.id)
      layer.current = null
    }
    // made once per id and type; the rest follows below
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [map, src, props.id, props.type])
  const seen = useRef(key)
  useLayoutEffect(() => {
    if (seen.current === key) return
    seen.current = key
    layer.current?.update(props)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key])
  return null
}
