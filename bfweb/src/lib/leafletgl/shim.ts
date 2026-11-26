// A MapLibre map, done with Leaflet, for browsers without WebGL -- DCS
// World's embedded one, where the cockpit panel shows the dashboard (see
// lib/webgl). The dashboard's map pages are written against react-map-gl:
// a <Map> with <Source>/<Layer> and DOM <Marker> children, and code that
// calls the map itself (project, flyTo, on('render'), getSource().setData,
// addImage, setPaintProperty ...). This is that surface, as far as those
// pages use it, so they draw everything they draw on MapLibre: the raster
// basemap (Esri, like every look the pages offer), GeoJSON line / fill /
// circle layers styled by MapLibre expressions (./style), icon symbols on a
// canvas, DOM markers, and the 2D overlays the pages paint themselves.
//
// What it leaves out: rotation and pitch (the pages turn both off), line
// blur, line offset and real line gradients (the trail draws in its end
// colour), symbol text and collision. Zoom is reported in MapLibre's
// 512-px units, so the pages' zoom thresholds mean the same here.
import L from 'leaflet'
import 'leaflet/dist/leaflet.css'
import type { Feature, FeatureCollection } from 'geojson'
import { colorOf, dashArray, evaluate, numberOf, passes } from './style'

/** MapLibre zoom z (512-px tiles) is Leaflet zoom z + 1 (256-px tiles). */
export const ZOOM_OFFSET = 1

type LngLatLike = [number, number] | { lng: number; lat: number } | { lon: number; lat: number }
type Fn = (e: Record<string, unknown>) => void
type Dict = Record<string, unknown>

function toLatLng(p: LngLatLike): L.LatLng {
  if (Array.isArray(p)) return L.latLng(p[1], p[0])
  return L.latLng(p.lat, 'lng' in p ? p.lng : p.lon)
}

type Anchor = 'center' | 'top' | 'bottom' | 'left' | 'right' | 'top-left' | 'top-right' | 'bottom-left' | 'bottom-right'
const ANCHOR: Record<Anchor, [string, string]> = {
  center: ['-50%', '-50%'], top: ['-50%', '0'], bottom: ['-50%', '-100%'], left: ['0', '-50%'], right: ['-100%', '-50%'],
  'top-left': ['0', '0'], 'top-right': ['-100%', '0'], 'bottom-left': ['0', '-100%'], 'bottom-right': ['-100%', '-100%'],
}

/** A DOM element pinned to a point, like maplibregl.Marker. */
export class DomMarker {
  readonly root = document.createElement('div')
  private readonly inner = document.createElement('div')
  private at: L.LatLng = L.latLng(0, 0)
  private readonly map: ShimMap

  constructor(map: ShimMap, el: HTMLElement, anchor: string = 'center', offset?: [number, number]) {
    this.map = map
    this.root.className = 'lgl-marker'
    this.root.style.cssText = 'position:absolute;left:0;top:0;width:0;height:0;'
    this.inner.style.cssText = 'position:absolute;left:0;top:0;'
    this.inner.appendChild(el)
    this.root.appendChild(this.inner)
    this.setAnchor(anchor, offset)
    map.markerPane.appendChild(this.root)
    map.markers.add(this)
  }

  setAnchor(anchor: string = 'center', offset?: [number, number]) {
    const [ax, ay] = ANCHOR[(anchor as Anchor) in ANCHOR ? (anchor as Anchor) : 'center']
    const [dx, dy] = offset ?? [0, 0]
    this.inner.style.transform = `translate(${ax}, ${ay}) translate(${dx}px, ${dy}px)`
    return this
  }

  setLngLat(p: LngLatLike) {
    this.at = toLatLng(p)
    this.place()
    return this
  }

  getLngLat() {
    return { lng: this.at.lng, lat: this.at.lat }
  }

  /** Layer points move only when the zoom does: ShimMap calls this then. */
  place() {
    const q = this.map.lmap.latLngToLayerPoint(this.at)
    this.root.style.transform = `translate3d(${q.x}px, ${q.y}px, 0)`
  }

  getElement() {
    return this.inner.firstElementChild as HTMLElement
  }

  addTo() {
    return this
  }

  remove() {
    this.root.remove()
    this.map.markers.delete(this)
    return this
  }
}

/** A GeoJSON source; layers redraw when its data changes. */
export class ShimSource {
  data: FeatureCollection = { type: 'FeatureCollection', features: [] }
  readonly listeners = new Set<() => void>()
  readonly id: string
  constructor(id: string) {
    this.id = id
  }
  setData(d: unknown) {
    this.data = asCollection(d)
    for (const l of this.listeners) l()
    return this
  }
}

function asCollection(d: unknown): FeatureCollection {
  const o = d as { type?: string; features?: Feature[] } | null
  if (o?.type === 'FeatureCollection') return { type: 'FeatureCollection', features: o.features ?? [] }
  if (o?.type === 'Feature') return { type: 'FeatureCollection', features: [o as unknown as Feature] }
  return { type: 'FeatureCollection', features: [] }
}

export interface LayerSpec {
  id: string
  type: string
  paint?: Dict
  layout?: Dict
  filter?: unknown
  minzoom?: number
  maxzoom?: number
}

/** One style layer, drawn by Leaflet (line, fill, circle) or on the symbol canvas (symbol). */
export class ShimLayer {
  paint: Dict
  layout: Dict
  filter: unknown
  minzoom?: number
  maxzoom?: number
  private overrides: Dict = {}
  private geo: L.GeoJSON | null = null
  private pane: HTMLElement | null = null
  private renderer: L.Renderer | null = null
  private readonly onData = () => this.rebuild()
  private readonly map: ShimMap
  readonly source: ShimSource
  readonly id: string
  readonly type: string

  constructor(map: ShimMap, source: ShimSource, id: string, type: string, spec: LayerSpec) {
    this.map = map
    this.source = source
    this.id = id
    this.type = type
    this.paint = spec.paint ?? {}
    this.layout = spec.layout ?? {}
    this.filter = spec.filter
    this.minzoom = spec.minzoom
    this.maxzoom = spec.maxzoom
    if (type !== 'symbol') {
      const name = `lgl-${map.nextPaneId()}`
      this.pane = map.lmap.createPane(name)
      this.pane.style.zIndex = String(401 + (map.paneCount % 180))
      this.pane.style.pointerEvents = 'none'
      const renderer = L.svg({ pane: name, padding: 0.5 })
      this.renderer = renderer
      this.geo = L.geoJSON(undefined, {
        pane: name,
        // Leaflet would make its own markers for points; circles are ours.
        pointToLayer: (_f, latlng) => L.circleMarker(latlng, { renderer, interactive: false }),
        style: (f) => this.styleFor(f as Feature),
        interactive: false,
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        ...({ renderer } as any),
      })
    }
    source.listeners.add(this.onData)
    this.rebuild()
  }

  update(spec: LayerSpec) {
    // React re-gave the layer its props: keep an override (setPaintProperty)
    // only for the keys React did not change.
    for (const k of Object.keys(this.overrides)) {
      if (JSON.stringify(spec.paint?.[k]) !== JSON.stringify(this.paint[k])) delete this.overrides[k]
    }
    this.paint = spec.paint ?? {}
    this.layout = spec.layout ?? {}
    this.filter = spec.filter
    this.minzoom = spec.minzoom
    this.maxzoom = spec.maxzoom
    this.rebuild()
  }

  setPaint(k: string, v: unknown) {
    this.overrides[k] = v
    this.restyle()
  }

  setLayout(k: string, v: unknown) {
    this.layout = { ...this.layout, [k]: v }
    this.rebuild()
  }

  getPaint(k: string): unknown {
    return k in this.overrides ? this.overrides[k] : this.paint[k]
  }

  shown(): boolean {
    const z = this.map.getZoom()
    if (this.layout.visibility === 'none') return false
    if (this.minzoom != null && z < this.minzoom) return false
    if (this.maxzoom != null && z >= this.maxzoom) return false
    return true
  }

  /** The features to draw now: filtered, and of a geometry this layer type draws. */
  features(): Feature[] {
    const z = this.map.getZoom()
    return this.source.data.features.filter((f) => {
      const g = f.geometry?.type ?? ''
      const ok = this.type === 'fill' ? /Polygon$/.test(g)
        : this.type === 'line' ? /(LineString|Polygon)$/.test(g)
          : /Point$/.test(g)
      return ok && passes(this.filter, f, z)
    })
  }

  rebuild() {
    if (this.type === 'symbol') {
      this.map.triggerRepaint()
      return
    }
    const geo = this.geo!
    geo.clearLayers()
    if (!this.shown()) {
      geo.remove()
      return
    }
    for (const f of this.features()) geo.addData(f)
    if (!this.map.lmap.hasLayer(geo)) geo.addTo(this.map.lmap)
  }

  /** Zoom-dependent paint, or a setPaintProperty: restyle what is drawn. */
  restyle() {
    if (this.type === 'symbol') {
      this.map.triggerRepaint()
      return
    }
    const geo = this.geo!
    if (this.shown() !== this.map.lmap.hasLayer(geo)) return this.rebuild()
    geo.setStyle((f) => this.styleFor(f as Feature))
  }

  private styleFor(f: Feature): L.PathOptions & { radius?: number } {
    const ctx = { zoom: this.map.getZoom(), feature: f }
    const p = (k: string) => this.getPaint(k)
    if (this.type === 'fill') {
      return {
        stroke: false,
        fill: true,
        fillColor: colorOf(p('fill-color'), ctx, '#000'),
        fillOpacity: numberOf(p('fill-opacity'), ctx, 1),
      }
    }
    if (this.type === 'circle') {
      const sw = numberOf(p('circle-stroke-width'), ctx, 0)
      return {
        radius: numberOf(p('circle-radius'), ctx, 5),
        fill: true,
        fillColor: colorOf(p('circle-color'), ctx, '#000'),
        fillOpacity: numberOf(p('circle-opacity'), ctx, 1),
        stroke: sw > 0,
        weight: sw,
        color: colorOf(p('circle-stroke-color'), ctx, '#000'),
        opacity: numberOf(p('circle-stroke-opacity'), ctx, 1),
      }
    }
    const width = numberOf(p('line-width'), ctx, 1)
    // A gradient has no Leaflet equivalent: draw it in its colour near the end.
    const grad = p('line-gradient')
    const color = grad != null
      ? colorOf(grad, { ...ctx, progress: 0.85 }, '#000')
      : colorOf(p('line-color'), ctx, '#000')
    return {
      stroke: true,
      fill: false,
      color,
      opacity: numberOf(p('line-opacity'), ctx, 1),
      weight: width,
      dashArray: dashArray(p('line-dasharray'), ctx, width),
      lineCap: (evaluate(this.layout['line-cap'], ctx) as L.LineCapShape) ?? 'butt',
      lineJoin: (evaluate(this.layout['line-join'], ctx) as L.LineJoinShape) ?? 'miter',
    }
  }

  /** Symbol layers draw their icons here, every frame the map renders. */
  drawSymbols(c: CanvasRenderingContext2D) {
    if (!this.shown()) return
    const z = this.map.getZoom()
    for (const f of this.features()) {
      const ctx = { zoom: z, feature: f }
      const id = evaluate(this.layout['icon-image'], ctx)
      const img = typeof id === 'string' ? this.map.images.get(id) : undefined
      if (!img) continue
      const coords = (f.geometry as { coordinates: number[] }).coordinates
      const q = this.map.lmap.latLngToContainerPoint([coords[1], coords[0]])
      const size = numberOf(this.layout['icon-size'], ctx, 1)
      const w = (img.canvas.width / img.ratio) * size
      const h = (img.canvas.height / img.ratio) * size
      c.save()
      c.globalAlpha = Math.max(0, Math.min(1, numberOf(this.getPaint('icon-opacity'), ctx, 1)))
      c.translate(q.x, q.y)
      c.rotate((numberOf(this.layout['icon-rotate'], ctx, 0) * Math.PI) / 180)
      c.drawImage(img.canvas, -w / 2, -h / 2, w, h)
      c.restore()
    }
  }

  remove() {
    this.source.listeners.delete(this.onData)
    this.geo?.remove()
    this.renderer?.remove()
    this.pane?.remove()
    this.map.triggerRepaint()
  }
}

interface ShimImage {
  canvas: HTMLCanvasElement
  ratio: number
}

/** The MapLibre map surface the dashboard's map pages use, over a Leaflet map. */
export class ShimMap {
  readonly isLeafletGL = true
  readonly sources = new Map<string, ShimSource>()
  readonly layers = new Map<string, ShimLayer>()
  readonly images = new Map<string, ShimImage>()
  readonly markers = new Set<DomMarker>()
  /** Fixed to the container (not moved by panning): symbols and the pages' own canvases. */
  readonly fixedPane: HTMLElement
  readonly markerPane: HTMLElement
  private readonly symbolCanvas = document.createElement('canvas')
  /** What getCanvas() returns: the pages put their overlay canvases after it. */
  private readonly placeholder = document.createElement('canvas')
  private readonly handlers = new Map<string, Set<Fn>>()
  private raf = 0
  private panes = 0
  readonly dragPan: { enable: () => void; disable: () => void; isEnabled: () => boolean }
  readonly scrollZoom: { enable: () => void; disable: () => void }
  readonly lmap: L.Map

  constructor(lmap: L.Map) {
    this.lmap = lmap
    this.fixedPane = lmap.createPane('lgl-fixed')
    this.fixedPane.style.zIndex = '590'
    this.fixedPane.style.pointerEvents = 'none'
    this.symbolCanvas.style.cssText = 'position:absolute;left:0;top:0;width:100%;height:100%;pointer-events:none;'
    this.placeholder.style.display = 'none'
    this.fixedPane.appendChild(this.symbolCanvas)
    this.fixedPane.appendChild(this.placeholder)
    this.markerPane = lmap.createPane('lgl-markers')
    this.markerPane.style.zIndex = '610'
    this.dragPan = {
      enable: () => lmap.dragging.enable(),
      disable: () => lmap.dragging.disable(),
      isEnabled: () => lmap.dragging.enabled(),
    }
    this.scrollZoom = { enable: () => lmap.scrollWheelZoom.enable(), disable: () => lmap.scrollWheelZoom.disable() }

    lmap.on('move', () => {
      this.fire('move')
      this.renderNow()
    })
    lmap.on('moveend', () => this.fire('moveend'))
    lmap.on('zoom viewreset', () => {
      for (const m of this.markers) m.place()
    })
    lmap.on('zoom', () => this.fire('zoom'))
    lmap.on('zoomstart', () => this.fire('zoomstart'))
    lmap.on('movestart', () => this.fire('movestart'))
    lmap.on('zoomend', () => {
      for (const l of this.layers.values()) l.restyle()
      this.fire('zoomend')
    })
    lmap.on('dragstart', () => this.fire('dragstart'))
    lmap.on('dragend', () => this.fire('dragend'))
    lmap.on('resize', () => {
      this.sizeFixed()
      this.fire('resize')
      this.renderNow()
    })
    lmap.on('click', (e: L.LeafletMouseEvent) => {
      // A marker handles its own clicks, as on MapLibre's DOM markers.
      if ((e.originalEvent.target as HTMLElement | null)?.closest?.('.lgl-marker')) return
      this.fire('click', {
        lngLat: { lng: e.latlng.lng, lat: e.latlng.lat },
        point: { x: e.containerPoint.x, y: e.containerPoint.y },
        originalEvent: e.originalEvent,
      })
    })
    this.sizeFixed()
    // Development only: the live stand-in, for poking at from the console.
    if (import.meta.env.DEV) (window as unknown as { __leafletgl?: ShimMap }).__leafletgl = this
  }

  nextPaneId(): number {
    return ++this.panes
  }

  get paneCount(): number {
    return this.panes
  }

  // ── events ──
  on(type: string, fn: Fn) {
    let s = this.handlers.get(type)
    if (!s) this.handlers.set(type, (s = new Set()))
    s.add(fn)
    return this
  }

  off(type: string, fn: Fn) {
    this.handlers.get(type)?.delete(fn)
    return this
  }

  once(type: string, fn: Fn) {
    const w: Fn = (e) => {
      this.off(type, w)
      fn(e)
    }
    return this.on(type, w)
  }

  fire(type: string, extra: Dict = {}) {
    const e = { type, target: this, ...extra }
    for (const fn of [...(this.handlers.get(type) ?? [])]) {
      try {
        fn(e)
      } catch (err) {
        console.error(`leafletgl: a '${type}' handler threw`, err)
      }
    }
    return this
  }

  // ── drawing ──
  private sizeFixed() {
    const s = this.lmap.getSize()
    this.fixedPane.style.width = `${s.x}px`
    this.fixedPane.style.height = `${s.y}px`
    const dpr = window.devicePixelRatio || 1
    this.symbolCanvas.width = Math.round(s.x * dpr)
    this.symbolCanvas.height = Math.round(s.y * dpr)
  }

  /** Keep the fixed pane at the container's corner however the map has panned. */
  private placeFixed() {
    L.DomUtil.setPosition(this.fixedPane, this.lmap.containerPointToLayerPoint([0, 0]))
  }

  private renderNow() {
    this.placeFixed()
    const c = this.symbolCanvas.getContext('2d')
    if (c) {
      const dpr = window.devicePixelRatio || 1
      c.setTransform(1, 0, 0, 1, 0, 0)
      c.clearRect(0, 0, this.symbolCanvas.width, this.symbolCanvas.height)
      c.setTransform(dpr, 0, 0, dpr, 0, 0)
      for (const l of this.layers.values()) if (l.type === 'symbol') l.drawSymbols(c)
    }
    this.fire('render')
  }

  triggerRepaint() {
    if (this.raf) return
    this.raf = requestAnimationFrame(() => {
      this.raf = 0
      this.renderNow()
    })
  }

  // ── camera ──
  getMap() {
    return this
  }

  getContainer(): HTMLElement {
    return this.lmap.getContainer()
  }

  getCanvas(): HTMLCanvasElement {
    return this.placeholder
  }

  getCanvasContainer(): HTMLElement {
    return this.lmap.getContainer()
  }

  project(p: LngLatLike) {
    const q = this.lmap.latLngToContainerPoint(toLatLng(p))
    return { x: q.x, y: q.y }
  }

  unproject(p: [number, number] | { x: number; y: number }) {
    const [x, y] = Array.isArray(p) ? p : [p.x, p.y]
    const g = this.lmap.containerPointToLatLng([x, y])
    return { lng: g.lng, lat: g.lat }
  }

  getZoom(): number {
    return this.lmap.getZoom() - ZOOM_OFFSET
  }

  getCenter() {
    const c = this.lmap.getCenter()
    return { lng: c.lng, lat: c.lat }
  }

  getBearing() {
    return 0
  }

  getPitch() {
    return 0
  }

  getBounds() {
    const b = this.lmap.getBounds()
    return {
      getWest: () => b.getWest(),
      getEast: () => b.getEast(),
      getSouth: () => b.getSouth(),
      getNorth: () => b.getNorth(),
      contains: (p: LngLatLike) => b.contains(toLatLng(p)),
      toArray: () => [[b.getWest(), b.getSouth()], [b.getEast(), b.getNorth()]],
    }
  }

  flyTo(o: { center?: LngLatLike; zoom?: number; duration?: number; animate?: boolean }) {
    const c = o.center ? toLatLng(o.center) : this.lmap.getCenter()
    const z = o.zoom != null ? o.zoom + ZOOM_OFFSET : this.lmap.getZoom()
    const ms = o.animate === false ? 0 : o.duration ?? 1000
    if (ms <= 0) this.lmap.setView(c, z, { animate: false })
    else this.lmap.flyTo(c, z, { duration: ms / 1000 })
    return this
  }

  easeTo(o: { center?: LngLatLike; zoom?: number; duration?: number }) {
    return this.flyTo(o)
  }

  jumpTo(o: { center?: LngLatLike; zoom?: number }) {
    return this.flyTo({ ...o, duration: 0 })
  }

  setZoom(z: number) {
    this.lmap.setZoom(z + ZOOM_OFFSET, { animate: false })
    return this
  }

  zoomTo(z: number) {
    return this.flyTo({ zoom: z, duration: 300 })
  }

  setCenter(c: LngLatLike) {
    return this.flyTo({ center: c, duration: 0 })
  }

  fitBounds(
    bounds: [[number, number], [number, number]] | { getWest(): number; getSouth(): number; getEast(): number; getNorth(): number },
    o: { padding?: number | { top?: number; bottom?: number; left?: number; right?: number }; maxZoom?: number; duration?: number; animate?: boolean } = {},
  ) {
    const [[w, s], [e, n]] = Array.isArray(bounds)
      ? bounds
      : [[bounds.getWest(), bounds.getSouth()], [bounds.getEast(), bounds.getNorth()]]
    const p = typeof o.padding === 'number'
      ? { top: o.padding, bottom: o.padding, left: o.padding, right: o.padding }
      : { top: 0, bottom: 0, left: 0, right: 0, ...(o.padding ?? {}) }
    this.lmap.fitBounds(L.latLngBounds([s, w], [n, e]), {
      paddingTopLeft: [p.left ?? 0, p.top ?? 0],
      paddingBottomRight: [p.right ?? 0, p.bottom ?? 0],
      maxZoom: o.maxZoom != null ? o.maxZoom + ZOOM_OFFSET : undefined,
      animate: o.animate !== false && (o.duration ?? 1) > 0,
    })
    return this
  }

  resize() {
    this.lmap.invalidateSize({ pan: false })
    return this
  }

  loaded() {
    return true
  }

  isStyleLoaded() {
    return true
  }

  // ── style ──
  getLayer(id: string) {
    return this.layers.get(id)
  }

  setPaintProperty(id: string, k: string, v: unknown) {
    this.layers.get(id)?.setPaint(k, v)
    return this
  }

  setLayoutProperty(id: string, k: string, v: unknown) {
    this.layers.get(id)?.setLayout(k, v)
    return this
  }

  getSource(id: string) {
    return this.sources.get(id)
  }

  hasImage(id: string) {
    return this.images.has(id)
  }

  addImage(id: string, img: ImageData | HTMLImageElement | HTMLCanvasElement | ImageBitmap | { width: number; height: number; data: Uint8Array | Uint8ClampedArray }, o: { pixelRatio?: number } = {}) {
    const cv = document.createElement('canvas')
    cv.width = img.width
    cv.height = img.height
    const c = cv.getContext('2d')
    if (c) {
      if ('data' in img) {
        const d = img instanceof ImageData ? img : new ImageData(new Uint8ClampedArray(img.data), img.width, img.height)
        c.putImageData(d, 0, 0)
      } else {
        c.drawImage(img as CanvasImageSource, 0, 0)
      }
    }
    this.images.set(id, { canvas: cv, ratio: o.pixelRatio ?? 1 })
    this.triggerRepaint()
    return this
  }

  removeImage(id: string) {
    this.images.delete(id)
    return this
  }

  /** maplibregl.Marker's job, for code that makes its own (see groundwar/engine). */
  addDomMarker(el: HTMLElement, anchor: string = 'center', offset?: [number, number]) {
    return new DomMarker(this, el, anchor, offset)
  }

  destroy() {
    cancelAnimationFrame(this.raf)
    for (const m of [...this.markers]) m.remove()
    this.handlers.clear()
  }
}


// ── the basemap ──

interface RasterSource {
  type?: string
  tiles?: string[]
  maxzoom?: number
  minzoom?: number
}
interface StyleLayer {
  id: string
  type: string
  source?: string
  minzoom?: number
  maxzoom?: number
  paint?: Dict
}
interface StyleSpec {
  sources?: Record<string, RasterSource>
  layers?: StyleLayer[]
}

const FALLBACK_TILES = 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}'

/** Raster paint as a CSS filter: what MapLibre does to the tiles, roughly. */
function rasterFilter(paint: Dict): string {
  const parts: string[] = []
  const sat = Number(paint['raster-saturation'] ?? 0)
  if (sat) parts.push(`saturate(${Math.max(0, 1 + sat)})`)
  const con = Number(paint['raster-contrast'] ?? 0)
  if (con) parts.push(`contrast(${Math.max(0, 1 + con)})`)
  const bmax = Number(paint['raster-brightness-max'] ?? 1)
  if (bmax !== 1) parts.push(`brightness(${Math.max(0, bmax)})`)
  return parts.join(' ')
}

/** Put a MapLibre raster style's layers on the map as tile layers; returns them. */
export function applyStyle(lmap: L.Map, holder: HTMLElement, style: unknown, old: L.TileLayer[]): L.TileLayer[] {
  for (const t of old) t.remove()
  const spec = (typeof style === 'object' && style ? style : {}) as StyleSpec
  const out: L.TileLayer[] = []
  let bg = '#0a0d07'
  ;(spec.layers ?? []).forEach((layer, i) => {
    if (layer.type === 'background') {
      bg = String(layer.paint?.['background-color'] ?? bg)
      return
    }
    if (layer.type !== 'raster' || !layer.source) return
    const src = spec.sources?.[layer.source]
    const url = src?.tiles?.[0]
    if (!url) return
    const t = L.tileLayer(url, {
      zIndex: i,
      opacity: Number(layer.paint?.['raster-opacity'] ?? 1),
      maxNativeZoom: src.maxzoom ?? 19,
      minZoom: layer.minzoom != null ? layer.minzoom + ZOOM_OFFSET : 0,
      maxZoom: layer.maxzoom != null ? layer.maxzoom + ZOOM_OFFSET : 22,
      crossOrigin: true,
    })
    t.addTo(lmap)
    const filter = rasterFilter(layer.paint ?? {})
    if (filter) t.getContainer()?.style.setProperty('filter', filter)
    out.push(t)
  })
  if (!out.length) out.push(L.tileLayer(FALLBACK_TILES, { maxNativeZoom: 19, maxZoom: 22, opacity: 0.85 }).addTo(lmap))
  holder.style.background = bg
  return out
}

