// The replay engine: owns the MapLibre map, the playback clock, the 2D
// layers, the 3D layer and the label overlay. React only renders controls
// and reads a throttled snapshot (`onUi`); nothing here re-renders React per
// frame.

import maplibregl, { type GeoJSONSource, type Map as MLMap, type StyleSpecification } from 'maplibre-gl'
import 'maplibre-gl/dist/maplibre-gl.css'
import {
  type Kind, type State, type TrackStore,
  isAir, isWeapon, sideHex, M_TO_FT,
} from './data'
import { Theater, type Cam3D } from './theater'

export type ViewMode = '2d' | '3d'
/** follow = keep it centred (2D) / orbit it (3D); chase, cockpit and padlock are 3D only. */
export type CamMode = Cam3D
export type Basemap = 'dark' | 'sat'

export interface ViewOpts {
  ground: boolean
  weapons: boolean
  labels: boolean
  /** Trail length behind each aircraft, ms (0 = off). */
  trail: number
}

export interface EngineUi {
  t: number
  playing: boolean
  speed: number
  buffering: boolean
  mode: ViewMode
  cam: CamMode
  basemap: Basemap
  focus: number | null
  focusState: State | null
  /** second object for the BRAA tool, and its state */
  measure: number | null
  measureState: State | null
  /** the next map click picks the measured object */
  measuring: boolean
  opts: ViewOpts
  error: string | null
}

const FOCUS_HEX = '#b6f04a'
const COLORS: Record<string, string> = { blue: '#4a8fd4', red: '#d24b4b', other: '#c9a227', focus: FOCUS_HEX }
const colorKey = (c?: string | null) => (c === 'Blue' ? 'blue' : c === 'Red' ? 'red' : 'other')

/** Kinds whose icon turns with the heading. */
const ROTATES: Partial<Record<Kind, true>> = { air: true, helo: true, missile: true, bomb: true, rocket: true, torpedo: true, ship: true, carrier: true, vehicle: true, armor: true }

function style(): StyleSpecification {
  return {
    version: 8,
    sources: {
      dark: {
        type: 'raster',
        tiles: ['https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}'],
        tileSize: 256,
        attribution: 'Esri',
      },
      sat: {
        type: 'raster',
        tiles: ['https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'],
        tileSize: 256,
        maxzoom: 17,
        attribution: 'Esri, Maxar, Earthstar Geographics',
      },
    },
    layers: [
      { id: 'bg', type: 'background', paint: { 'background-color': '#0a0d07' } },
      { id: 'dark', type: 'raster', source: 'dark', paint: { 'raster-opacity': 0.9 } },
      { id: 'sat', type: 'raster', source: 'sat', layout: { visibility: 'none' }, paint: { 'raster-saturation': -0.25, 'raster-brightness-max': 0.85 } },
    ],
  }
}

// ── icons ──────────────────────────────────────────────────────────────

type Shape = 'jet' | 'helo' | 'missile' | 'bomb' | 'vehicle' | 'armor' | 'sam' | 'infantry' | 'static' | 'ship' | 'carrier'
const SHAPE: Record<Kind, Shape> = {
  air: 'jet', helo: 'helo', missile: 'missile', bomb: 'bomb', rocket: 'missile', torpedo: 'missile',
  sam: 'sam', armor: 'armor', vehicle: 'vehicle', infantry: 'infantry', static: 'static', ship: 'ship', carrier: 'carrier',
}

function drawShape(ctx: CanvasRenderingContext2D, shape: Shape, s: number) {
  const c = s / 2
  ctx.beginPath()
  switch (shape) {
    case 'jet':
      ctx.moveTo(c, s * 0.04)
      ctx.lineTo(c + s * 0.07, s * 0.36)
      ctx.lineTo(c + s * 0.44, s * 0.6)
      ctx.lineTo(c + s * 0.44, s * 0.68)
      ctx.lineTo(c + s * 0.08, s * 0.6)
      ctx.lineTo(c + s * 0.06, s * 0.8)
      ctx.lineTo(c + s * 0.2, s * 0.92)
      ctx.lineTo(c - s * 0.2, s * 0.92)
      ctx.lineTo(c - s * 0.06, s * 0.8)
      ctx.lineTo(c - s * 0.08, s * 0.6)
      ctx.lineTo(c - s * 0.44, s * 0.68)
      ctx.lineTo(c - s * 0.44, s * 0.6)
      ctx.lineTo(c - s * 0.07, s * 0.36)
      ctx.closePath()
      break
    case 'helo':
      ctx.ellipse(c, s * 0.42, s * 0.14, s * 0.22, 0, 0, Math.PI * 2)
      ctx.rect(c - s * 0.04, s * 0.6, s * 0.08, s * 0.32)
      ctx.rect(c - s * 0.42, s * 0.4, s * 0.84, s * 0.05)
      ctx.rect(c - s * 0.025, s * 0.02, s * 0.05, s * 0.8)
      break
    case 'missile':
      ctx.moveTo(c, s * 0.1)
      ctx.lineTo(c + s * 0.06, s * 0.3)
      ctx.lineTo(c + s * 0.06, s * 0.7)
      ctx.lineTo(c + s * 0.18, s * 0.9)
      ctx.lineTo(c - s * 0.18, s * 0.9)
      ctx.lineTo(c - s * 0.06, s * 0.7)
      ctx.lineTo(c - s * 0.06, s * 0.3)
      ctx.closePath()
      break
    case 'bomb':
      ctx.ellipse(c, c, s * 0.12, s * 0.26, 0, 0, Math.PI * 2)
      break
    case 'vehicle':
      ctx.rect(s * 0.3, s * 0.22, s * 0.4, s * 0.56)
      break
    case 'armor':
      ctx.rect(s * 0.26, s * 0.2, s * 0.48, s * 0.6)
      ctx.moveTo(c, s * 0.02)
      ctx.lineTo(c, s * 0.3)
      break
    case 'sam':
      ctx.moveTo(c, s * 0.12)
      ctx.lineTo(s * 0.86, s * 0.82)
      ctx.lineTo(s * 0.14, s * 0.82)
      ctx.closePath()
      break
    case 'infantry':
      ctx.arc(c, c, s * 0.14, 0, Math.PI * 2)
      break
    case 'static':
      ctx.rect(s * 0.34, s * 0.34, s * 0.32, s * 0.32)
      break
    case 'ship':
      ctx.moveTo(c, s * 0.04)
      ctx.lineTo(c + s * 0.14, s * 0.3)
      ctx.lineTo(c + s * 0.14, s * 0.94)
      ctx.lineTo(c - s * 0.14, s * 0.94)
      ctx.lineTo(c - s * 0.14, s * 0.3)
      ctx.closePath()
      break
    case 'carrier':
      ctx.moveTo(c - s * 0.1, s * 0.02)
      ctx.lineTo(c + s * 0.16, s * 0.02)
      ctx.lineTo(c + s * 0.16, s * 0.98)
      ctx.lineTo(c - s * 0.16, s * 0.98)
      ctx.closePath()
      break
  }
}

function addIcons(map: MLMap) {
  const px = 44 // 22 css px at pixelRatio 2
  for (const shape of Object.keys({ jet: 1, helo: 1, missile: 1, bomb: 1, vehicle: 1, armor: 1, sam: 1, infantry: 1, static: 1, ship: 1, carrier: 1 }) as Shape[]) {
    for (const [ck, hex] of Object.entries(COLORS)) {
      const cv = document.createElement('canvas')
      cv.width = cv.height = px
      const ctx = cv.getContext('2d')!
      const small = shape === 'missile' || shape === 'bomb' || shape === 'infantry' || shape === 'static'
      const s = small ? px * 0.7 : px
      ctx.translate((px - s) / 2, (px - s) / 2)
      drawShape(ctx, shape, s)
      ctx.lineJoin = 'round'
      ctx.lineWidth = 4
      ctx.strokeStyle = 'rgba(0,0,0,0.75)'
      ctx.stroke()
      const hollow = shape === 'vehicle' || shape === 'sam'
      if (hollow) {
        ctx.lineWidth = 2.5
        ctx.strokeStyle = hex
        ctx.stroke()
      } else {
        ctx.fillStyle = hex
        ctx.fill()
      }
      const img = ctx.getImageData(0, 0, px, px)
      const name = `rp-${shape}-${ck}`
      if (!map.hasImage(name)) map.addImage(name, { width: px, height: px, data: new Uint8Array(img.data.buffer) }, { pixelRatio: 2 })
    }
  }
}

// ── engine ─────────────────────────────────────────────────────────────

interface Drawn {
  idx: number
  kind: Kind
  color: string
  ck: string
  st: State
  focus: boolean
}

const EMPTY: GeoJSON.FeatureCollection = { type: 'FeatureCollection', features: [] }

export class ReplayEngine {
  readonly map: MLMap
  readonly store: TrackStore
  readonly w0: number
  readonly w1: number
  t: number
  playing = false
  speed = 1
  mode: ViewMode = '2d'
  cam: CamMode = 'free'
  basemap: Basemap = 'dark'
  focus: number | null = null
  measure: number | null = null
  measuring = false
  opts: ViewOpts = { ground: true, weapons: true, labels: true, trail: 60_000 }
  onUi: ((ui: EngineUi) => void) | null = null

  /** The 3D view, made the first time someone switches to it. */
  private theater: Theater | null = null
  private mapEl: HTMLDivElement
  private labelsEl: HTMLDivElement
  private labelPool: HTMLDivElement[] = []
  private raf = 0
  private lastFrame = 0
  private lastUi = 0
  private lastSlow = -Infinity
  private slowDirty = true
  private buffering = false
  private ready = false
  private centred = false
  private destroyed = false

  constructor(container: HTMLDivElement, labelsEl: HTMLDivElement, store: TrackStore, w0: number, w1: number, t: number) {
    this.store = store
    this.w0 = w0
    this.w1 = w1
    this.t = Math.min(Math.max(t, w0), w1)
    this.labelsEl = labelsEl
    this.mapEl = container
    this.map = new maplibregl.Map({
      container,
      style: style(),
      center: [42, 42],
      zoom: 6,
      maxPitch: 85,
      attributionControl: { compact: true },
      // the 3D layer reads the GL canvas back for nothing, but a resize
      // without this flickers on some drivers
      preserveDrawingBuffer: false,
    })
    this.map.addControl(new maplibregl.NavigationControl({ visualizePitch: true }), 'top-right')
    this.map.addControl(new maplibregl.ScaleControl({ unit: 'nautical' }), 'bottom-left')
    this.map.on('load', () => this.onLoad())
    this.map.on('dragstart', e => {
      if ((e as { originalEvent?: unknown }).originalEvent && this.cam !== 'free') this.setCam('free')
    })
    // While paused the map still moves under the user; 'render' (not 'move')
    // so 3D labels use the positions the layer has just projected.
    this.map.on('render', () => { if (!this.playing && this.mode === '2d') this.drawLabels() })
    this.map.on('click', e => this.pick(e.point.x, e.point.y, (e.originalEvent as MouseEvent).shiftKey))
    store.onChange = () => { this.slowDirty = true; this.kick() }
  }

  destroy() {
    this.destroyed = true
    cancelAnimationFrame(this.raf)
    this.store.onChange = null
    this.theater?.destroy()
    this.map.remove()
  }

  private onLoad() {
    const map = this.map
    addIcons(map)
    map.addSource('rp-trails', { type: 'geojson', data: EMPTY })
    map.addSource('rp-slow', { type: 'geojson', data: EMPTY })
    map.addSource('rp-dyn', { type: 'geojson', data: EMPTY })
    map.addSource('rp-measure', { type: 'geojson', data: EMPTY })
    map.addLayer({
      id: 'rp-trails', type: 'line', source: 'rp-trails',
      layout: { 'line-cap': 'round', 'line-join': 'round' },
      paint: { 'line-color': ['get', 'color'], 'line-opacity': ['get', 'op'], 'line-width': ['get', 'w'] },
    })
    map.addLayer({
      id: 'rp-measure', type: 'line', source: 'rp-measure',
      paint: { 'line-color': '#ffffff', 'line-width': 1.5, 'line-dasharray': [3, 2], 'line-opacity': 0.9 },
    })
    const symbol = (id: string, source: string) => map.addLayer({
      id, type: 'symbol', source,
      layout: {
        'icon-image': ['get', 'icon'],
        'icon-rotate': ['get', 'rot'],
        'icon-rotation-alignment': 'map',
        'icon-allow-overlap': true,
        'icon-ignore-placement': true,
        'symbol-sort-key': ['get', 'z'],
      },
    })
    symbol('rp-slow', 'rp-slow')
    map.addLayer({
      id: 'rp-focus', type: 'circle', source: 'rp-dyn', filter: ['==', ['get', 'focus'], 1],
      paint: { 'circle-radius': 17, 'circle-color': 'rgba(0,0,0,0)', 'circle-stroke-color': FOCUS_HEX, 'circle-stroke-width': 1.5, 'circle-stroke-opacity': 0.9 },
    })
    symbol('rp-dyn', 'rp-dyn')
    this.ready = true
    this.applyMode()
    this.applyBasemap()
    this.kick()
  }

  // ── controls ──

  setTime(t: number) {
    this.t = Math.min(Math.max(t, this.w0), this.w1)
    this.slowDirty = true
    this.changed()
  }
  play() {
    if (this.t >= this.w1) this.t = this.w0
    this.playing = true
    this.changed()
  }
  pause() { this.playing = false; this.changed() }
  toggle() { if (this.playing) this.pause(); else this.play() }
  setSpeed(s: number) { this.speed = s; this.changed() }
  setFocus(idx: number | null) {
    this.focus = idx
    if (idx == null || idx === this.measure) this.measure = null
    if (idx != null) {
      this.store.wholeTrack(idx)
      if (this.cam === 'free') this.cam = 'follow'
      this.centreOn(idx)
    } else if (this.cam !== 'free') {
      this.cam = 'free'
    }
    this.slowDirty = true
    this.changed()
  }
  /** Pick the object the BRAA tool measures to (null clears it). */
  setMeasure(idx: number | null) {
    this.measure = idx
    this.measuring = false
    if (idx == null && this.cam === 'padlock') this.cam = 'chase'
    if (idx != null) this.store.wholeTrack(idx)
    this.changed()
  }
  /** The next click on the map picks the measured object. */
  startMeasuring(on = true) {
    this.measuring = on
    this.changed()
  }
  setCam(c: CamMode) {
    this.cam = this.focus == null ? 'free' : c
    if (this.cam === 'padlock' && this.measure == null) this.cam = 'chase'
    this.map.scrollZoom.enable(this.cam === 'free' ? undefined : { around: 'center' })
    if (this.cam === 'free') this.map.dragPan.enable()
    this.changed()
  }
  setMode(m: ViewMode) {
    this.mode = m
    if (m === '2d' && this.cam !== 'free') this.cam = 'follow'
    if (this.ready) this.applyMode()
    this.changed()
  }
  setBasemap(b: Basemap) {
    this.basemap = b
    if (this.ready) this.applyBasemap()
    this.changed()
  }
  setOpts(o: Partial<ViewOpts>) {
    this.opts = { ...this.opts, ...o }
    this.slowDirty = true
    this.changed()
  }

  /** Screen space the panels cover, so following keeps the aircraft in
   *  the visible part of the map rather than under the side panel. */
  setPadding(p: { top: number; right: number; bottom: number; left: number }) {
    this.map.setPadding(p)
    this.kick()
  }

  private applyBasemap() {
    const map = this.map
    map.setLayoutProperty('dark', 'visibility', this.basemap === 'dark' ? 'visible' : 'none')
    map.setLayoutProperty('sat', 'visibility', this.basemap === 'sat' ? 'visible' : 'none')
  }

  private applyMode() {
    const three = this.mode === '3d'
    if (three && !this.theater && this.mapEl.parentElement) {
      const th = new Theater(this.mapEl.parentElement)
      // The theatre sits under the panels and the label overlay.
      this.mapEl.parentElement.insertBefore(th.el, this.mapEl.nextSibling)
      th.onChange = () => this.kick()
      th.el.addEventListener('click', ev => {
        const r = th.el.getBoundingClientRect()
        this.pick(ev.clientX - r.left, ev.clientY - r.top, ev.shiftKey)
      })
      this.theater = th
    }
    if (this.theater) this.theater.el.style.display = three ? '' : 'none'
    this.mapEl.style.visibility = three ? 'hidden' : ''
    if (!three) this.map.resize()
    this.slowDirty = true
  }

  private centreOn(idx: number) {
    const st = this.store.state(idx, this.t)
    if (st) {
      this.map.jumpTo({ center: [st.lon, st.lat], zoom: Math.max(this.map.getZoom(), 9) })
      this.centred = true
    }
  }

  // ── the loop ──

  /** A control changed: tell the UI now (not on the next frame, which a
   *  hidden tab may never draw) and draw. */
  private changed() {
    this.emitUi()
    this.kick()
  }

  /** Make sure a frame is coming. */
  kick() {
    if (this.destroyed || this.raf) return
    this.raf = requestAnimationFrame(now => this.frame(now))
  }

  private frame(now: number) {
    this.raf = 0
    if (this.destroyed) return
    const dt = this.lastFrame ? Math.min(now - this.lastFrame, 250) : 0
    this.lastFrame = now
    this.store.around(this.t)
    this.buffering = !this.store.isLoaded(this.t)
    if (this.playing && !this.buffering) {
      this.t += dt * this.speed
      if (this.t >= this.w1) {
        this.t = this.w1
        this.playing = false
      }
    }
    if (this.ready) {
      if (!this.centred) this.firstCentre()
      this.draw(now)
    }
    if (now - this.lastUi > 100 || !this.playing) {
      this.lastUi = now
      this.emitUi()
    }
    if (this.playing) this.kick()
    else this.lastFrame = 0
  }

  private emitUi() {
    this.onUi?.({
      t: this.t,
      playing: this.playing,
      speed: this.speed,
      buffering: this.buffering,
      mode: this.mode,
      cam: this.cam,
      basemap: this.basemap,
      focus: this.focus,
      focusState: this.focus != null ? this.store.state(this.focus, this.t) : null,
      measure: this.measure,
      measureState: this.measure != null ? this.store.state(this.measure, this.t) : null,
      measuring: this.measuring,
      opts: this.opts,
      error: this.store.error,
    })
  }

  /** Frame the action the first time there is data: the focus aircraft,
   *  or the box around every aircraft in the air. */
  private firstCentre() {
    if (this.focus != null) {
      this.centreOn(this.focus)
      return
    }
    const ids = this.store.present(this.t)
    if (!ids.length) return
    const b = new maplibregl.LngLatBounds()
    let any = false
    for (const idx of ids) {
      const o = this.store.meta.objects[idx]
      if (!isAir(o.k)) continue
      const st = this.store.state(idx, this.t)
      if (st) { b.extend([st.lon, st.lat]); any = true }
    }
    if (!any) {
      for (const idx of ids.slice(0, 500)) {
        const st = this.store.state(idx, this.t)
        if (st) { b.extend([st.lon, st.lat]); any = true }
      }
    }
    if (any) {
      this.map.fitBounds(b, { padding: 80, maxZoom: 10, duration: 0 })
      this.centred = true
    }
  }

  private collect(): { dyn: Drawn[]; slow: Drawn[] } {
    const t = this.t
    const { objects } = this.store.meta
    const dyn: Drawn[] = []
    const slow: Drawn[] = []
    for (const idx of this.store.present(t)) {
      const o = objects[idx]
      if (t < o.t0 || t > o.t1) continue
      const air = isAir(o.k), weapon = isWeapon(o.k)
      if (weapon && !this.opts.weapons) continue
      if (!air && !weapon && !this.opts.ground) continue
      const st = this.store.state(idx, t)
      if (!st) continue
      const focus = idx === this.focus
      const ck = focus ? 'focus' : colorKey(o.c)
      const d: Drawn = { idx, kind: o.k, color: sideHex(o.c), ck, st, focus }
      ;(air || weapon ? dyn : slow).push(d)
    }
    if (this.measure != null && this.measure !== this.focus && !dyn.some(d => d.idx === this.measure)) {
      const o = objects[this.measure]
      const st = this.store.state(this.measure, t)
      if (o && st) (isAir(o.k) || isWeapon(o.k) ? dyn : slow).push({ idx: this.measure, kind: o.k, color: sideHex(o.c), ck: colorKey(o.c), st, focus: false })
    }
    // The focus always draws, even if its kind is filtered out.
    if (this.focus != null && !dyn.some(d => d.focus)) {
      const o = objects[this.focus]
      const st = this.store.state(this.focus, t)
      if (o && st) dyn.push({ idx: this.focus, kind: o.k, color: sideHex(o.c), ck: 'focus', st, focus: true })
    }
    return { dyn, slow }
  }

  private trails(dyn: Drawn[]): { pts: [number, number, number][]; color: string; op: number; w: number }[] {
    const out = []
    const t = this.t
    for (const d of dyn) {
      if (d.focus) {
        // Whole flight so far.
        const o = this.store.meta.objects[d.idx]
        out.push({ pts: this.store.trail(d.idx, t, t - o.t0, 1000), color: FOCUS_HEX, op: 0.95, w: 2.2 })
      } else if (isAir(d.kind) && this.opts.trail > 0) {
        out.push({ pts: this.store.trail(d.idx, t, this.opts.trail, 1000), color: d.color, op: 0.55, w: 1.4 })
      } else if (isWeapon(d.kind)) {
        out.push({ pts: this.store.trail(d.idx, t, 20_000), color: '#e9e4d0', op: 0.7, w: 1.2 })
      }
    }
    return out
  }

  private draw(now: number) {
    const { dyn, slow } = this.collect()
    const trails = this.trails(dyn)
    const fs = this.focus != null ? this.store.state(this.focus, this.t) : null
    const ms = this.measure != null ? this.store.state(this.measure, this.t) : null
    const line: [number, number, number][] | null = fs && ms ? [[fs.lon, fs.lat, fs.alt], [ms.lon, ms.lat, ms.alt]] : null
    if (this.mode === '2d') {
      const feat = (d: Drawn): GeoJSON.Feature => ({
        type: 'Feature',
        geometry: { type: 'Point', coordinates: [d.st.lon, d.st.lat] },
        properties: {
          idx: d.idx,
          icon: `rp-${SHAPE[d.kind]}-${d.ck}`,
          rot: ROTATES[d.kind] ? d.st.hdg : 0,
          focus: d.focus ? 1 : 0,
          z: d.focus ? 1e9 : isAir(d.kind) ? d.st.alt : 0,
        },
      })
      ;(this.map.getSource('rp-dyn') as GeoJSONSource | undefined)?.setData({ type: 'FeatureCollection', features: dyn.map(feat) })
      if (this.slowDirty || now - this.lastSlow > 1000) {
        this.lastSlow = now
        this.slowDirty = false
        ;(this.map.getSource('rp-slow') as GeoJSONSource | undefined)?.setData({ type: 'FeatureCollection', features: slow.map(feat) })
      }
      ;(this.map.getSource('rp-measure') as GeoJSONSource | undefined)?.setData(line
        ? { type: 'Feature', geometry: { type: 'LineString', coordinates: line.map(p => [p[0], p[1]]) }, properties: {} }
        : EMPTY)
      ;(this.map.getSource('rp-trails') as GeoJSONSource | undefined)?.setData({
        type: 'FeatureCollection',
        features: trails.filter(tr => tr.pts.length > 1).map(tr => ({
          type: 'Feature',
          geometry: { type: 'LineString', coordinates: tr.pts.map(p => [p[0], p[1]]) },
          properties: { color: tr.color, op: tr.op, w: tr.w },
        })),
      })
    } else if (this.theater) {
      const { objects } = this.store.meta
      this.theater.render({
        objects: [...slow, ...dyn].map(d => ({ idx: d.idx, kind: d.kind, name: objects[d.idx]?.n, color: d.color, st: d.st, focus: d.focus })),
        trails: trails.map(tr => ({ pts: tr.pts, color: tr.color, opacity: tr.op })),
        line,
        focus: this.focus,
        measure: this.measure,
        cam: this.cam,
      })
    }
    if (this.mode === '2d') this.camera()
    this.drawLabels(dyn)
  }

  /** 2D: keep the followed aircraft centred (the theatre moves its own
   *  camera in 3D). */
  private camera() {
    if (this.focus == null || this.cam === 'free') return
    const st = this.store.state(this.focus, this.t)
    if (st) this.map.jumpTo({ center: [st.lon, st.lat] })
  }

  private label(i: number): HTMLDivElement {
    while (this.labelPool.length <= i) {
      const el = document.createElement('div')
      el.className = 'rp-label'
      this.labelsEl.appendChild(el)
      this.labelPool.push(el)
    }
    return this.labelPool[i]
  }

  private drawLabels(dyn?: Drawn[]) {
    const { objects } = this.store.meta
    const items = (dyn ?? this.collect().dyn).filter(d => isAir(d.kind))
    let n = 0
    if (this.opts.labels || this.focus != null) {
      const pos = new Map<number, { x: number; y: number }>()
      if (this.mode === '3d' && this.theater) for (const p of this.theater.projected) pos.set(p.idx, p)
      for (const d of items) {
        if (!this.opts.labels && !d.focus && d.idx !== this.measure) continue
        let xy = pos.get(d.idx)
        if (this.mode === '2d') xy = this.map.project([d.st.lon, d.st.lat])
        if (!xy) continue
        const o = objects[d.idx]
        const el = this.label(n++)
        const name = o.p ?? (o.n ?? '').replace(/_/g, ' ')
        const fl = Math.round((d.st.alt * M_TO_FT) / 100)
        const text = `${name}  ${String(fl).padStart(3, '0')}`
        if (el.textContent !== text) el.textContent = text
        el.style.transform = `translate(${Math.round(xy.x + 14)}px, ${Math.round(xy.y - 8)}px)`
        el.style.color = d.focus ? FOCUS_HEX : d.color
        el.style.display = ''
        el.classList.toggle('rp-label-focus', d.focus)
      }
    }
    for (let i = n; i < this.labelPool.length; i++) this.labelPool[i].style.display = 'none'
  }

  private pick(x: number, y: number, shift = false) {
    let best: { idx: number; d: number } | null = null
    if (this.mode === '2d') {
      const feats = this.map.queryRenderedFeatures([[x - 12, y - 12], [x + 12, y + 12]], { layers: ['rp-dyn', 'rp-slow'] })
      for (const f of feats) {
        const idx = Number(f.properties?.idx)
        const o = this.store.meta.objects[idx]
        if (!o) continue
        // Prefer aircraft over whatever they are flying over.
        const d = isAir(o.k) ? 0 : 1
        if (!best || d < best.d) best = { idx, d }
      }
    } else if (this.theater) {
      const idx = this.theater.pick(x, y)
      if (idx != null) best = { idx, d: 0 }
    }
    if (!best) return
    if ((shift || this.measuring) && best.idx !== this.focus && this.focus != null) this.setMeasure(best.idx)
    else this.setFocus(best.idx)
  }
}

