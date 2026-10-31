// The replay engine: owns the playback clock, the MapLibre basemap (2D), the
// screen overlay and the 3D theatre. React only renders controls and reads a
// throttled snapshot (`onUi`); nothing here re-renders React per frame.
//
// It plays any `TrackSource` -- a server recording streamed in chunks or a
// local .acmi read whole in the browser -- the way Tacview does: every object
// in the picture, labelled, with trails, lock lines, radar sweeps, threat
// rings and the bullseye, from a top-down map or any 3D camera.

import maplibregl, { type Map as MLMap, type StyleSpecification } from 'maplibre-gl'
import 'maplibre-gl/dist/maplibre-gl.css'
import {
  type Kind, type State, type TrackSource,
  isAir, isWeapon, isEffect, isSurface, distM, bearingDeg,
} from './data'
import { Theater, type Cam3D, type TheaterFrame } from './theater'
import { braa, gAt, iasOf, machOf, nearest, velAt } from './analysis'
import type { HudData } from './hud'
import { Overlay, SIDE_COLOR, sideKey, type Item, type Polyline, type Ring, type Text2D, type Wedge } from './overlay'
import { threatOf } from './threats'
import { type Units, fmtDist, fmtSpeed, labelAlt, labelSpeed } from './units'
import { airfields, type Airfield } from './dcs/theatre'
import { shortNameOf } from '../../../shared/models/catalog'

export type ViewMode = '2d' | '3d'
/** follow = keep it centred (2D) / orbit it (3D); the rest are 3D only. */
export type CamMode = Cam3D
/** dark and satellite rasters, or the F10-style topographic map */
export type Basemap = 'dark' | 'sat' | 'topo'

export interface ViewOpts {
  labels: boolean
  weapons: boolean
  ground: boolean
  /** flares, chaff, shells, explosions */
  effects: boolean
  /** trail length behind each aircraft, ms (0 = off) */
  trail: number
  /** 3D: Tacview's smoke ribbons (wing-oriented bands) instead of thin trails */
  ribbons: boolean
  /** lines from each shooter to what its radar has locked */
  locks: boolean
  /** radar sweeps, when the recording has them */
  radar: boolean
  /** engagement rings round air defences */
  threats: boolean
  /** range rings round each coalition's bullseye */
  bullseye: boolean
  /** the HUD over the cockpit camera */
  hud: boolean
  /** which coalitions are drawn (the followed object always is) */
  sides: { blue: boolean; red: boolean; other: boolean }
  /** where each aircraft will be in 20 s at its present velocity */
  vectors: boolean
  /** 3D: a line from each aircraft down to the ground */
  verticals: boolean
  /** map: latitude/longitude grid */
  grid: boolean
  /** objects drawn this much bigger than life-size-or-minimum (Tacview's exaggeration) */
  scale: number
  units: Units
  /** label lines: name only, + type/level/speed, + Mach/AOA */
  labelDetail: 'min' | 'std' | 'full'
  /** airfields and runways on the map */
  airfields: boolean
}

/** A point-to-point measurement on the map. */
export interface RulerUi { a: [number, number]; b: [number, number] | null; dist: number; brg: number }

export const DEFAULT_OPTS: ViewOpts = {
  labels: true, weapons: true, ground: true, effects: true, trail: 60_000, ribbons: true,
  locks: true, radar: true, threats: false, bullseye: true, hud: true,
  sides: { blue: true, red: true, other: true }, vectors: false, verticals: false, grid: false,
  scale: 1, units: 'imperial', labelDetail: 'std', airfields: true,
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
  /** the next click picks the measured object */
  measuring: boolean
  opts: ViewOpts
  /** objects in the picture now, by coalition */
  counts: { blue: number; red: number; other: number }
  /** the ruler tool is armed (clicks place its ends) */
  ruling: boolean
  ruler: RulerUi | null
  error: string | null
}

const BULLSEYE_NM = [20, 40, 60, 80, 100]
const NM = 1852
const DEG = Math.PI / 180

/** Ctrl-click (Cmd-click on a Mac) picks the object to measure to, as in Tacview. */
export const versusClick = (ev: MouseEvent) => ev.ctrlKey || ev.metaKey
const LOCK_KEYS = ['LockedTarget', 'LockedTarget2', 'LockedTarget3', 'LockedTarget4', 'LockedTarget5', 'LockedTarget6', 'LockedTarget7', 'LockedTarget8', 'LockedTarget9']

/** The F10-style map: OpenFreeMap's vector tiles (OpenStreetMap; free, no
 *  key) with hillshade from the same elevation tiles as the 3D terrain. */
const TOPO_STYLE = 'https://tiles.openfreemap.org/styles/liberty'

function withHillshade(next: StyleSpecification): StyleSpecification {
  const layers = [...next.layers]
  const at = layers.findIndex(l => l.type === 'symbol')
  layers.splice(at < 0 ? layers.length : at, 0, {
    id: 'rp-hills', type: 'hillshade', source: 'rp-dem',
    paint: { 'hillshade-exaggeration': 0.55, 'hillshade-shadow-color': '#3b3424', 'hillshade-highlight-color': '#fffdf5' },
  })
  return {
    ...next,
    sources: {
      ...next.sources,
      'rp-dem': { type: 'raster-dem', tiles: ['https://tiles.mapterhorn.com/{z}/{x}/{y}.webp'], tileSize: 512, encoding: 'terrarium', maxzoom: 12, attribution: '© Mapterhorn' },
    },
    layers,
  }
}

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

/** One object in the picture this frame, before projection. */
interface Drawn {
  idx: number
  kind: Kind
  side: ReturnType<typeof sideKey>
  st: State
  focus: boolean
  target: boolean
}

const shortType = (n?: string | null) => (n ?? '').replace(/_/g, ' ').replace(/\s+/g, ' ').trim()

export class ReplayEngine {
  readonly map: MLMap
  readonly store: TrackSource
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
  opts: ViewOpts = { ...DEFAULT_OPTS }
  onUi: ((ui: EngineUi) => void) | null = null

  /** The 3D view, made the first time someone switches to it. */
  private theater: Theater | null = null
  private overlay: Overlay
  private mapEl: HTMLDivElement
  private root: HTMLElement
  private raf = 0
  private lastFrame = 0
  private lastUi = 0
  private buffering = false
  private ready = false
  private centred = false
  private destroyed = false
  /** what was drawn last, in screen pixels, for picking */
  private hits: { idx: number; x: number; y: number; pri: number }[] = []
  private counts = { blue: 0, red: 0, other: 0 }
  private scratch = new Float32Array(4096)
  /** ruler: armed, its ends, and the live end under the cursor */
  ruling = false
  private rulerA: [number, number] | null = null
  private rulerB: [number, number] | null = null
  private cursor: [number, number] | null = null
  /** where the cursor is on the map, for the coordinate readout */
  onCursor: ((lonlat: [number, number] | null) => void) | null = null
  private fields: Airfield[] = []
  private topo = false
  /** the recording's in-game clock at t = 0 (unix ms), for the sun */
  private sunBase: number | null

  constructor(container: HTMLDivElement, store: TrackSource, w0: number, w1: number, t: number) {
    this.store = store
    this.w0 = w0
    this.w1 = w1
    this.t = Math.min(Math.max(t, w0), w1)
    this.mapEl = container
    this.root = container.parentElement ?? container
    const ref = store.meta.reference_time ? Date.parse(store.meta.reference_time) : NaN
    this.sunBase = Number.isFinite(ref) ? ref + (store.meta.first_frame_s ?? 0) * 1000 : store.meta.start_ms || null
    this.map = new maplibregl.Map({
      container,
      style: style(),
      center: [42, 42],
      zoom: 6,
      maxPitch: 0,
      dragRotate: false,
      pitchWithRotate: false,
      attributionControl: { compact: true },
    })
    this.map.touchZoomRotate.disableRotation()
    this.map.addControl(new maplibregl.ScaleControl({ unit: 'nautical' }), 'bottom-left')
    this.overlay = new Overlay(this.root)
    // style.load, not load: load waits for every basemap tile, and a slow tile
    // server would hold the whole viewer up.
    this.map.once('style.load', () => {
      this.ready = true
      this.applyMode()
      this.applyBasemap()
      this.kick()
    })
    this.map.on('dragstart', e => {
      if ((e as { originalEvent?: unknown }).originalEvent && this.cam !== 'free') this.setCam('free')
    })
    // The overlay is drawn when the map draws, so the two never drift apart
    // -- whether the clock moved it or the user is panning.
    this.map.on('render', () => { if (this.mode === '2d') this.paint2D() })
    this.map.on('click', e => {
      if (this.ruling) this.rulerClick([e.lngLat.lng, e.lngLat.lat])
      else this.pick(e.point.x, e.point.y, versusClick(e.originalEvent))
    })
    // Ctrl with the right button picks the versus object too (and on a Mac
    // Ctrl-click arrives as a context menu).
    this.map.on('contextmenu', e => {
      if (!versusClick(e.originalEvent)) return
      e.originalEvent.preventDefault()
      this.pick(e.point.x, e.point.y, true)
    })
    this.map.on('mousemove', e => {
      this.cursor = [e.lngLat.lng, e.lngLat.lat]
      this.onCursor?.(this.cursor)
      if (this.ruling && this.rulerA && !this.rulerB) { this.map.triggerRepaint(); this.emitUi() }
    })
    this.map.on('mouseout', () => { this.cursor = null; this.onCursor?.(null) })
    store.onChange = () => this.kick()
    void airfields().then(f => { this.fields = f; this.kick(); if (this.mode === '2d') this.map.triggerRepaint() }).catch(() => {})
  }

  destroy() {
    this.destroyed = true
    cancelAnimationFrame(this.raf)
    this.store.onChange = null
    this.theater?.destroy()
    this.overlay.destroy()
    this.map.remove()
  }

  // ── controls ──

  setTime(t: number) {
    this.t = Math.min(Math.max(t, this.w0), this.w1)
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
  /** The next click picks the measured object. */
  startMeasuring(on = true) {
    this.measuring = on
    this.changed()
  }
  setCam(c: CamMode) {
    this.cam = this.focus == null ? 'free' : c
    if (this.cam === 'padlock' && this.measure == null) this.cam = 'chase'
    if (this.mode === '2d' && this.cam !== 'free') this.cam = 'follow'
    this.map.scrollZoom.enable(this.cam === 'free' ? undefined : { around: 'center' })
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
    this.changed()
  }
  /** Arm or disarm the ruler; disarming clears it. */
  setRuling(on: boolean) {
    this.ruling = on
    if (!on) { this.rulerA = null; this.rulerB = null }
    else if (this.mode === '3d') this.setMode('2d')
    this.map.getCanvas().style.cursor = on ? 'crosshair' : ''
    this.changed()
  }
  private rulerClick(ll: [number, number]) {
    if (!this.rulerA || this.rulerB) { this.rulerA = ll; this.rulerB = null }
    else this.rulerB = ll
    this.changed()
  }
  private rulerUi(): RulerUi | null {
    const a = this.rulerA
    const b = this.rulerB ?? (this.ruling ? this.cursor : null)
    if (!a) return null
    if (!b) return { a, b: null, dist: 0, brg: 0 }
    return { a, b, dist: distM(a[0], a[1], b[0], b[1]), brg: bearingDeg(a[0], a[1], b[0], b[1]) }
  }

  /** Jump to the next (1) or previous (-1) event worth seeing: kills,
   *  shots, losses and bookmarks -- the followed object's own when there is
   *  one. Lands a few seconds before it. */
  jumpEvent(dir: 1 | -1) {
    const lead = 5_000
    const f = this.focus
    const evs = this.store.meta.events.filter(ev =>
      (ev.k === 'kill' || ev.k === 'destroyed' || ev.k === 'lost' || ev.k === 'fired' || ev.k === 'bookmark') &&
      (f == null || ev.o === f || ev.by === f || ev.tg === f || ev.k === 'bookmark'))
    const now = this.t + lead
    const next = dir > 0
      ? evs.find(ev => ev.t > now + 500)
      : [...evs].reverse().find(ev => ev.t < now - 1_500)
    if (next) this.setTime(next.t - lead)
  }

  /** Step one sample-ish forward or back, paused. */
  step(ms: number) {
    this.pause()
    this.setTime(this.t + ms)
  }

  /** Screen space the panels cover, so following keeps the aircraft in
   *  the visible part of the map rather than under a panel. */
  setPadding(p: { top: number; right: number; bottom: number; left: number }) {
    this.map.setPadding(p)
    this.kick()
  }

  private applyBasemap() {
    const map = this.map
    const topo = this.basemap === 'topo'
    if (topo !== this.topo) {
      this.topo = topo
      if (topo) map.setStyle(TOPO_STYLE, { transformStyle: (_prev, next) => withHillshade(next) })
      else {
        map.setStyle(style())
        map.once('style.load', () => this.applyBasemap())
      }
      this.root.classList.toggle('rp-light', topo)
      return
    }
    if (topo || !map.getLayer('dark')) return
    map.setLayoutProperty('dark', 'visibility', this.basemap === 'dark' ? 'visible' : 'none')
    map.setLayoutProperty('sat', 'visibility', this.basemap === 'sat' ? 'visible' : 'none')
  }

  private applyMode() {
    const three = this.mode === '3d'
    if (three && !this.theater) {
      const th = new Theater(this.root)
      // The theatre sits over the map, under the overlay and the panels.
      this.root.insertBefore(th.el, this.overlay.el)
      th.onChange = () => this.kick()
      th.onUserMove = () => { if (this.cam !== 'free' && this.cam !== 'follow') this.setCam('follow') }
      const pickAt = (ev: MouseEvent, versus: boolean) => {
        const r = th.el.getBoundingClientRect()
        this.pick(ev.clientX - r.left, ev.clientY - r.top, versus)
      }
      th.el.addEventListener('click', ev => pickAt(ev, versusClick(ev)))
      th.el.addEventListener('contextmenu', ev => { if (versusClick(ev)) { ev.preventDefault(); pickAt(ev, true) } })
      this.theater = th
    }
    if (this.theater) this.theater.el.style.display = three ? '' : 'none'
    this.mapEl.style.visibility = three ? 'hidden' : ''
    if (!three) this.map.resize()
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
      if (this.mode === '2d') {
        // Move the camera, then let the map's own render draw the overlay.
        this.camera2D()
        this.map.triggerRepaint()
      } else {
        this.paint3D()
      }
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
      counts: { ...this.counts },
      ruling: this.ruling,
      ruler: this.rulerUi(),
      error: this.store.error,
    })
  }

  /** Frame the action the first time there is data: the focus aircraft,
   *  or the box round every aircraft in the air. */
  private firstCentre() {
    if (this.focus != null) {
      this.centreOn(this.focus)
      return
    }
    const ids = this.store.present(this.t)
    if (!ids.length) return
    const b = new maplibregl.LngLatBounds()
    let any = false
    for (const pass of [true, false]) {
      for (const idx of ids) {
        const o = this.store.meta.objects[idx]
        if (!o || (pass && !isAir(o.k)) || isEffect(o.k) || o.k === 'bullseye') continue
        const st = this.store.state(idx, this.t)
        if (st) { b.extend([st.lon, st.lat]); any = true }
      }
      if (any) break
    }
    if (any) {
      this.map.fitBounds(b, { padding: 80, maxZoom: 10, duration: 0 })
      this.centred = true
    }
  }

  /** Everything in the picture now, filtered by the layer toggles. */
  private collect(): Drawn[] {
    const t = this.t
    const { objects } = this.store.meta
    const out: Drawn[] = []
    const counts = { blue: 0, red: 0, other: 0 }
    for (const idx of this.store.present(t)) {
      const o = objects[idx]
      if (!o || t < o.t0 || t > o.t1) continue
      const focus = idx === this.focus, target = idx === this.measure
      if (!focus && !target) {
        if (isWeapon(o.k) && !this.opts.weapons) continue
        if (isEffect(o.k) && !this.opts.effects) continue
        if (isSurface(o.k) && !this.opts.ground) continue
        if (o.k === 'bullseye' && !this.opts.bullseye) continue
        if (!this.opts.sides[sideKey(o.c)]) continue
      }
      const st = this.store.state(idx, t)
      if (!st) continue
      const side = sideKey(o.c)
      if (isAir(o.k)) counts[side]++
      out.push({ idx, kind: o.k, side, st, focus, target })
    }
    this.counts = counts
    return out
  }

  /** Label text, Tacview-style: who, then type / flight level / speed. */
  private labelOf(d: Drawn): [string, string, number] {
    const o = this.store.meta.objects[d.idx]
    const type = shortNameOf(o.n) || shortType(o.n) || d.kind
    if (isAir(d.kind)) {
      const who = (o.p ?? o.cs ?? '').trim()
      const pri = 500 + (who ? 400 : 0) + d.st.alt / 100
      if (this.opts.labelDetail === 'min') return [who || type, '', pri]
      const u = this.opts.units
      const bits = [labelAlt(d.st.alt, u), labelSpeed(iasOf(d.st), u)]
      if (this.opts.labelDetail === 'full') {
        bits.push(`M${machOf(d.st).toFixed(2)}`)
        if (d.st.aoa != null) bits.push(`α${d.st.aoa.toFixed(0)}`)
        const g = d.st.ch?.VerticalGForce
        if (g != null) bits.push(`${g.toFixed(1)}G`)
      }
      return [who || type, who ? `${type}  ${bits.join('  ')}` : bits.join('  '), pri]
    }
    if (isWeapon(d.kind)) return [type, '', 200]
    if (d.focus || d.target) return [type, o.g ?? '', 0]
    return ['', '', 0]
  }

  private toItem(d: Drawn, x: number, y: number): Item {
    const [line1, line2, pri] = this.labelOf(d)
    const kindPri = isAir(d.kind) ? 3 : isWeapon(d.kind) ? 2 : isEffect(d.kind) ? 0 : 1
    return {
      idx: d.idx, kind: d.kind, side: d.side, st: d.st, x, y, focus: d.focus, target: d.target,
      line1, line2,
      pri: d.focus ? 1e9 : d.target ? 1e8 : pri + kindPri * 1000,
    }
  }

  /** Trail points, newest last: the whole flight so far for the focus,
   *  `opts.trail` for other aircraft, 20 s of smoke for weapons. */
  private trailSpec(d: Drawn): { len: number; step: number; color: string; alpha: number; width: number } | null {
    const o = this.store.meta.objects[d.idx]
    // a point every half second, thinning out so no trail passes ~600 points
    const step = (len: number) => Math.max(500, Math.round(len / 600))
    if (d.focus) return { len: this.t - o.t0, step: step(this.t - o.t0), color: SIDE_COLOR.focus, alpha: 0.95, width: 2.2 }
    if (isAir(d.kind) && this.opts.trail > 0) return { len: this.opts.trail, step: step(this.opts.trail), color: SIDE_COLOR[d.side], alpha: 0.6, width: 1.4 }
    if (isWeapon(d.kind)) return { len: 20_000, step: 0, color: '#eae6d6', alpha: 0.75, width: 1.2 }
    if (d.kind === 'flare' && this.opts.effects) return { len: 2_500, step: 0, color: '#fff3d0', alpha: 0.6, width: 1.6 }
    return null
  }

  /** Locked targets of a drawn object, as meta indices. */
  private locksOf(d: Drawn): number[] {
    const ch = d.st.ch
    if (!ch) return []
    const out: number[] = []
    for (const k of LOCK_KEYS) {
      const v = ch[k]
      if (v != null && v >= 0) out.push(Math.round(v))
    }
    return out
  }

  // ── 2D ──

  private camera2D() {
    if (this.focus == null || this.cam === 'free') return
    const st = this.store.state(this.focus, this.t)
    if (st) this.map.jumpTo({ center: [st.lon, st.lat] })
  }

  private paint2D() {
    if (!this.ready || this.destroyed) return
    const map = this.map
    const drawn = this.collect()
    const items: Item[] = []
    const byIdx = new Map<number, Item>()
    for (const d of drawn) {
      const p = map.project([d.st.lon, d.st.lat])
      const it = this.toItem(d, p.x, p.y)
      items.push(it)
      byIdx.set(d.idx, it)
    }
    items.sort((a, b) => a.pri - b.pri)

    const zoom = map.getZoom()
    const centreLat = map.getCenter().lat
    const pxPerM = (lat: number) => (512 * 2 ** zoom) / (40_075_016.686 * Math.cos(lat * DEG))

    const lines: Polyline[] = []
    const texts: Text2D[] = []
    if (this.opts.airfields) this.runways(lines, texts, zoom, pxPerM)
    let used = 0
    const take = (n: number): Float32Array => {
      if (used + n * 2 > this.scratch.length) {
        this.scratch = new Float32Array(Math.max(this.scratch.length * 2, used + n * 2))
        used = 0 // the old buffer stays with the lines made from it
      }
      const a = this.scratch.subarray(used, used + n * 2)
      used += n * 2
      return a
    }
    for (const d of drawn) {
      const spec = this.trailSpec(d)
      if (!spec || spec.len <= 0) continue
      const pts = this.store.trail(d.idx, this.t, spec.len, spec.step)
      if (pts.length < 2) continue
      const a = take(pts.length)
      for (let i = 0; i < pts.length; i++) {
        const p = map.project([pts[i][0], pts[i][1]])
        a[i * 2] = p.x; a[i * 2 + 1] = p.y
      }
      lines.push({ pts: a, n: pts.length, color: spec.color, alpha: spec.alpha, width: spec.width })
    }
    // Lock lines.
    if (this.opts.locks) {
      for (const d of drawn) {
        for (const tg of this.locksOf(d)) {
          const a = byIdx.get(d.idx), b = byIdx.get(tg)
          if (!a || !b) continue
          lines.push({ pts: new Float32Array([a.x, a.y, b.x, b.y]), n: 2, color: SIDE_COLOR[d.side], alpha: 0.85, width: 1.2, dash: [5, 4] })
        }
      }
    }
    // BRAA line to the measured object.
    if (this.focus != null && this.measure != null) {
      const a = byIdx.get(this.focus), b = byIdx.get(this.measure)
      if (a && b) {
        lines.push({ pts: new Float32Array([a.x, a.y, b.x, b.y]), n: 2, color: '#ffffff', alpha: 0.9, width: 1.5, dash: [4, 3] })
        const tag = this.braaTag(a, b)
        if (tag) texts.push(tag)
      }
    }

    const rings: Ring[] = []
    const wedges: Wedge[] = []
    for (const d of drawn) {
      const it = byIdx.get(d.idx)!
      const o = this.store.meta.objects[d.idx]
      if (this.opts.threats) {
        const th = threatOf(d.kind, o.n)
        if (th) rings.push({ x: it.x, y: it.y, r: th.range * pxPerM(d.st.lat), color: SIDE_COLOR[d.side], alpha: th.short ? 0.35 : 0.6, label: th.label, dash: th.short })
      }
      if (d.kind === 'bullseye' && this.opts.bullseye) {
        for (const nm of BULLSEYE_NM) {
          rings.push({ x: it.x, y: it.y, r: nm * NM * pxPerM(d.st.lat), color: SIDE_COLOR[d.side], alpha: 0.32, label: nm === BULLSEYE_NM[0] ? 'BULLSEYE' : `${nm}` })
        }
      }
      if (this.opts.radar) {
        const r = this.radarOf(d)
        if (r) {
          const a = (r.az - 90) * DEG
          wedges.push({ x: it.x, y: it.y, r: r.range * pxPerM(centreLat), from: a - (r.hbw / 2) * DEG, to: a + (r.hbw / 2) * DEG, color: SIDE_COLOR[d.side] })
        }
      }
    }

    // Velocity vectors: 20 s ahead at the present ground track and speed.
    if (this.opts.vectors) {
      for (const d of drawn) {
        if (!isAir(d.kind) && !isWeapon(d.kind)) continue
        const it = byIdx.get(d.idx)!
        const [lon, lat] = this.ahead(d.st, 20)
        const q = map.project([lon, lat])
        lines.push({ pts: new Float32Array([it.x, it.y, q.x, q.y]), n: 2, color: d.focus ? SIDE_COLOR.focus : SIDE_COLOR[d.side], alpha: 0.7, width: 1 })
      }
    }
    if (this.opts.grid) this.grid(lines, texts)
    const r = this.rulerUi()
    if (r) {
      const a = map.project(r.a)
      if (r.b) {
        const b = map.project(r.b)
        lines.push({ pts: new Float32Array([a.x, a.y, b.x, b.y]), n: 2, color: '#ffe680', alpha: 1, width: 1.6 })
        texts.push({ x: (a.x + b.x) / 2 + 8, y: (a.y + b.y) / 2 - 8, text: `${fmtDist(r.dist, this.opts.units)}  ${String(Math.round(r.brg) % 360).padStart(3, '0')}°`, color: '#ffe680', box: true })
      }
      rings.push({ x: a.x, y: a.y, r: 4, color: '#ffe680', alpha: 1 })
    }

    const ov = this.overlay
    ov.begin()
    ov.draw2D({ items, lines, rings, wedges, texts, labels: this.opts.labels, scale: this.opts.scale })
    this.hits = items.filter(it => !isEffect(it.kind)).map(it => ({ idx: it.idx, x: it.x, y: it.y, pri: it.pri }))
  }

  /** Range and closure, on the middle of the line between the followed
   *  object and the measured one (at those screen points). */
  private braaTag(a: { x: number; y: number }, b: { x: number; y: number }): Text2D | null {
    const fs = this.focus != null ? this.store.state(this.focus, this.t) : null
    const ms = this.measure != null ? this.store.state(this.measure, this.t) : null
    if (!fs || !ms) return null
    const r = braa(fs, ms)
    const u = this.opts.units
    // Close together, the middle of the line is under their labels: below them instead.
    const near = Math.hypot(a.x - b.x, a.y - b.y) < 90
    const x = near ? Math.min(a.x, b.x) - 20 : (a.x + b.x) / 2 + 8
    const y = near ? Math.max(a.y, b.y) + 34 : (a.y + b.y) / 2 - 8
    return { x, y, text: `${fmtDist(r.range, u)} · Vc ${fmtSpeed(r.closure, u)}`, color: '#ffffff', box: true }
  }

  /** Where a moving object will be `s` seconds on, [lon, lat, alt]. */
  private ahead(st: State, s: number): [number, number, number] {
    const d = st.gs * s
    const h = st.hdg * DEG
    return [
      st.lon + (Math.sin(h) * d) / (111_320 * Math.cos(st.lat * DEG)),
      st.lat + (Math.cos(h) * d) / 110_540,
      st.alt + st.vs * s,
    ]
  }

  /** Runways, at their real width once that is more than a couple of pixels,
   *  and airfield names as the F10 map shows them. */
  private runways(lines: Polyline[], texts: Text2D[], zoom: number, pxPerM: (lat: number) => number) {
    if (zoom < 5.5 || !this.fields.length) return
    const b = this.map.getBounds()
    const pad = 0.2
    const color = this.topo ? '#4a4a48' : '#cfd2c8'
    const named = zoom >= 7.2
    for (const f of this.fields) {
      if (f.lat < b.getSouth() - pad || f.lat > b.getNorth() + pad || f.lon < b.getWest() - pad || f.lon > b.getEast() + pad) continue
      for (const r of f.runways) {
        const p = this.map.project([r[3], r[2]]), q = this.map.project([r[5], r[4]])
        const w = Math.max(2, (r[7] || 150) * 0.3048 * pxPerM(f.lat))
        lines.push({ pts: new Float32Array([p.x, p.y, q.x, q.y]), n: 2, color, alpha: 0.9, width: w })
      }
      if (named) {
        const c = this.map.project([f.lon, f.lat])
        texts.push({ x: c.x + 10, y: c.y + 14, text: f.name, color: this.topo ? '#202020' : '#e8eadf', alpha: 0.85 })
      }
    }
  }

  /** Latitude/longitude lines across the view, spaced for the zoom, labelled
   *  at the left and top edges. */
  private grid(lines: Polyline[], texts: Text2D[]) {
    const map = this.map
    const b = map.getBounds()
    const span = Math.max(b.getEast() - b.getWest(), b.getNorth() - b.getSouth())
    const steps = [10, 5, 2, 1, 0.5, 0.25, 1 / 6, 1 / 12, 1 / 30, 1 / 60]
    const step = steps.find(x => span / x >= 4) ?? steps[steps.length - 1]
    const fmt = (v: number, pos: string, neg: string) => {
      const a = Math.abs(v), d = Math.floor(a + 1e-9), m = Math.round((a - d) * 60)
      return `${d}°${m ? String(m).padStart(2, '0') + "'" : ''}${v >= 0 ? pos : neg}`
    }
    const color = 'rgba(200,210,190,0.9)'
    for (let lon = Math.ceil(b.getWest() / step) * step; lon <= b.getEast(); lon += step) {
      const p = map.project([lon, b.getNorth()]), q = map.project([lon, b.getSouth()])
      lines.push({ pts: new Float32Array([p.x, p.y, q.x, q.y]), n: 2, color, alpha: 0.18, width: 1 })
      texts.push({ x: p.x + 3, y: 70, text: fmt(lon, 'E', 'W'), color, alpha: 0.7 })
    }
    for (let lat = Math.ceil(b.getSouth() / step) * step; lat <= b.getNorth(); lat += step) {
      const p = map.project([b.getWest(), lat]), q = map.project([b.getEast(), lat])
      lines.push({ pts: new Float32Array([p.x, p.y, q.x, q.y]), n: 2, color, alpha: 0.18, width: 1 })
      texts.push({ x: Math.max(p.x, 0) + 4, y: p.y - 3, text: fmt(lat, 'N', 'S'), color, alpha: 0.7 })
    }
  }

  /** The radar sweep an object's recording describes, if its radar is on. */
  private radarOf(d: Drawn): { az: number; el: number; range: number; hbw: number; vbw: number } | null {
    const ch = d.st.ch
    if (!ch || !(ch.RadarMode > 0) || !(ch.RadarRange > 0)) return null
    return {
      az: d.st.hdg + (ch.RadarAzimuth ?? 0),
      el: d.st.pitch + (ch.RadarElevation ?? 0),
      range: ch.RadarRange,
      hbw: ch.RadarHorizontalBeamwidth ?? 120,
      vbw: ch.RadarVerticalBeamwidth ?? 30,
    }
  }

  // ── 3D ──

  private paint3D() {
    const th = this.theater
    if (!th) return
    const drawn = this.collect()
    const { objects } = this.store.meta
    const fs = this.focus != null ? this.store.state(this.focus, this.t) : null
    const ms = this.measure != null ? this.store.state(this.measure, this.t) : null

    const trails: TheaterFrame['trails'] = []
    const ribbons: TheaterFrame['ribbons'] = []
    for (const d of drawn) {
      const spec = this.trailSpec(d)
      if (!spec || spec.len <= 0) continue
      // From behind or inside the followed jet its own ribbon would fill the
      // screen: it gets the thin trail there instead.
      const inside = d.focus && (this.cam === 'chase' || this.cam === 'cockpit' || this.cam === 'padlock')
      if (this.opts.ribbons && isAir(d.kind) && !inside) {
        ribbons.push({ pts: this.ribbon(d.idx, Math.min(spec.len, d.focus ? 120_000 : this.opts.trail || 30_000)), color: spec.color, opacity: d.focus ? 0.55 : spec.alpha * 0.8 })
        if (!d.focus) continue
      }
      trails.push({ pts: this.store.trail(d.idx, this.t, spec.len, spec.step), color: spec.color, opacity: spec.alpha })
    }
    const present = new Set(drawn.map(d => d.idx))
    const locks: TheaterFrame['locks'] = []
    if (this.opts.locks) {
      for (const d of drawn) for (const tg of this.locksOf(d)) if (present.has(tg)) locks.push({ a: d.idx, b: tg, color: SIDE_COLOR[d.side] })
    }
    const rings: TheaterFrame['rings'] = []
    const cones: TheaterFrame['cones'] = []
    for (const d of drawn) {
      const o = objects[d.idx]
      if (this.opts.threats) {
        const th3 = threatOf(d.kind, o.n)
        if (th3) rings.push({ idx: d.idx, r: th3.range, color: SIDE_COLOR[d.side], opacity: th3.short ? 0.3 : 0.55, dome: !th3.short })
      }
      if (d.kind === 'bullseye' && this.opts.bullseye) {
        for (const nm of BULLSEYE_NM) rings.push({ idx: d.idx, r: nm * NM, color: SIDE_COLOR[d.side], opacity: 0.3, dome: false })
      }
      if (this.opts.radar) {
        const r = this.radarOf(d)
        if (r) cones.push({ idx: d.idx, ...r, color: SIDE_COLOR[d.side] })
      }
    }
    const vectors: TheaterFrame['vectors'] = []
    if (this.opts.vectors) {
      for (const d of drawn) {
        if (!isAir(d.kind) && !isWeapon(d.kind)) continue
        vectors.push({ a: [d.st.lon, d.st.lat, d.st.alt], b: this.ahead(d.st, 20), color: d.focus ? SIDE_COLOR.focus : SIDE_COLOR[d.side] })
      }
    }
    th.render({
      vectors,
      verticals: this.opts.verticals,
      scale: this.opts.scale,
      sunMs: this.sunBase != null ? this.sunBase + this.t : null,
      objects: drawn.map(d => ({
        idx: d.idx, kind: d.kind, name: objects[d.idx]?.n,
        color: SIDE_COLOR[d.side], st: d.st, focus: d.focus, target: d.target,
      })),
      trails,
      ribbons,
      locks,
      rings,
      cones,
      line: fs && ms ? [[fs.lon, fs.lat, fs.alt], [ms.lon, ms.lat, ms.alt]] : null,
      focus: this.focus,
      measure: this.measure,
      cam: this.cam,
      hud: this.cam === 'cockpit' && this.opts.hud && fs ? this.hudData(fs, ms) : null,
    })
    // Labels over the scene, at the positions the theatre just projected.
    const pos = new Map<number, { x: number; y: number }>()
    for (const p of th.projected) pos.set(p.idx, p)
    const items: Item[] = []
    for (const d of drawn) {
      const p = pos.get(d.idx)
      if (!p) continue
      if (this.cam === 'cockpit' && d.focus) continue
      items.push(this.toItem(d, p.x, p.y))
    }
    const ov = this.overlay
    ov.begin()
    ov.labels(items, this.opts.labels)
    const fp = this.focus != null ? pos.get(this.focus) : undefined, mp = this.measure != null ? pos.get(this.measure) : undefined
    const tag = fp && mp && this.cam !== 'cockpit' ? this.braaTag(fp, mp) : null
    if (tag) ov.texts([tag])
    this.hits = items.filter(it => !isEffect(it.kind)).map(it => ({ idx: it.idx, x: it.x, y: it.y, pri: it.pri }))
  }

  /** A ribbon: position and roll every 500 ms (fewer for long ones) over the last `len` ms. */
  private ribbon(idx: number, len: number): [number, number, number, number][] {
    const out: [number, number, number, number][] = []
    const o = this.store.meta.objects[idx]
    const from = Math.max(o.t0, this.t - len)
    const step = Math.max(500, Math.round(len / 300)) // long ribbons thin out, as trails do
    for (let t = Math.ceil(from / step) * step; t < this.t; t += step) {
      const st = this.store.state(idx, t)
      if (st) out.push([st.lon, st.lat, st.alt, st.roll])
    }
    const now = this.store.state(idx, this.t)
    if (now) out.push([now.lon, now.lat, now.alt, now.roll])
    return out
  }

  /** What the cockpit HUD shows for the followed aircraft at t. */
  private hudData(fs: State, ms: State | null): HudData {
    const whole = this.focus != null ? this.store.wholeTrack(this.focus) : null
    const i = whole ? nearest(whole, this.t) : -1
    const ground = this.theater?.heightAt(fs.lon, fs.lat)
    let target: HudData['target'] = null
    if (ms && this.measure != null) {
      const o = this.store.meta.objects[this.measure]
      const dx = (ms.lon - fs.lon) * 111_320 * Math.cos(((fs.lat + ms.lat) / 2) * DEG)
      const dy = (ms.lat - fs.lat) * 110_540
      target = { label: (o?.p ?? o?.n ?? 'TGT').replace(/_/g, ' ').toUpperCase().slice(0, 18), range: Math.hypot(dx, dy, ms.alt - fs.alt) }
    }
    return {
      st: fs,
      ias: iasOf(fs),
      iasRecorded: fs.ias != null,
      mach: machOf(fs),
      g: whole ? gAt(whole, i) : null,
      vel: whole ? velAt(whole, this.t) : null,
      agl: fs.agl ?? (ground != null ? fs.alt - ground : null),
      target,
    }
  }

  /** The drawn object nearest a screen point (aircraft over what they fly
   *  over); Ctrl-click (or the measure tool) picks the versus object. */
  private pick(x: number, y: number, versus = false) {
    let best: { idx: number; d: number; pri: number } | null = null
    for (const h of this.hits) {
      const d = Math.hypot(h.x - x, h.y - y)
      if (d > 18) continue
      const score = d - (h.pri >= 3000 ? 8 : 0)
      if (!best || score < best.d) best = { idx: h.idx, d: score, pri: h.pri }
    }
    if (!best) return
    if ((versus || this.measuring) && best.idx !== this.focus && this.focus != null) this.setMeasure(best.idx)
    else this.setFocus(best.idx)
  }
}
