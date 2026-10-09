// The parts of the battlefield that move every frame, kept out of React:
//
//  * vehicles -- every vehicle of every formation in one GeoJSON symbol layer,
//    eased from where the last frame put it to where this one does, so a
//    column rolls rather than hops;
//  * live players -- maplibre markers moved every frame, interpolated between
//    pictures and dead-reckoned from heading and speed so they fly smoothly;
//  * an overlay canvas between the map and the DOM markers for what WebGL
//    layers do badly: fog of war, territory, battle effects (muzzle flashes,
//    bursts, drifting smoke), fire lines, order previews and range rings;
//  * the flowing dash on planned routes.
import maplibregl, { type GeoJSONSource, type Map as MlMap } from 'maplibre-gl'
import type { Feature, FeatureCollection, Point } from 'geojson'
import type { GroundPicture, LatLon, LivePlayer } from '../../api'
import { dist, lerp, lerpHeading, offset, offsetEN } from './geo'
import { playerSvg, vehicleImageId } from './sprites'
import { ATTACK, FIRE, NEAR_ZOOM, PENCIL, SIDE_BRIGHT, SIDE_COLOR, WITHDRAW, other, type Side } from './theme'

export type Verb = 'attack' | 'defend' | 'withdraw' | 'raise' | 'invalid'
export const VERB_COLOR: Record<Verb, string> = {
  attack: ATTACK,
  defend: PENCIL,
  withdraw: WITHDRAW,
  raise: PENCIL,
  invalid: '#9a9a90',
}

export interface Hover {
  /** Cursor, map-container pixels. */
  x: number
  y: number
  obj: number | null
  verb: Verb | null
}

export const VEH_SOURCE = 'gw-veh'
export const PATH_FLOW_LAYER = 'gw-path-flow'
export const EMPTY_FC: FeatureCollection = { type: 'FeatureCollection', features: [] }

const DASH_SEQ: number[][] = [
  [0, 4, 3], [0.5, 4, 2.5], [1, 4, 2], [1.5, 4, 1.5], [2, 4, 1], [2.5, 4, 0.5], [3, 4, 0],
  [0, 0.5, 3, 3.5], [0, 1, 3, 3], [0, 1.5, 3, 2.5], [0, 2, 3, 2], [0, 2.5, 3, 1.5], [0, 3, 3, 1], [0, 3.5, 3, 0.5],
]

interface Track {
  from: LatLon
  to: LatLon
  h0: number
  h1: number
  t0: number
  dur: number
}
interface VehTrack extends Track {
  /** Where the latest picture put it (`to` may be extrapolated past this). */
  real?: LatLon
  icon: string
  fid: number
  sel: boolean
  op: number
  enemy: boolean
}
interface PlayerTrack extends Track {
  marker: maplibregl.Marker
  el: HTMLDivElement
  ico: HTMLDivElement
  tag: HTMLDivElement
  mps: number
  cat: LivePlayer['category']
  self: boolean
  label: string
}
interface Particle {
  kind: 'flash' | 'boom' | 'smoke' | 'scorch'
  pos: LatLon
  born: number
  life: number
  /** Metres at birth (smoke grows). */
  m: number
  minPx: number
}

const now = () => performance.now()

function trackPos(t: Track, at: number): { pos: LatLon; hdg: number } {
  const k = t.dur > 0 ? Math.min(1, Math.max(0, (at - t.t0) / t.dur)) : 1
  const e = k < 1 ? k * (2 - k) * 0.35 + k * 0.65 : 1 // mostly linear, a touch of ease-out
  return { pos: lerp(t.from, t.to, e), hdg: lerpHeading(t.h0, t.h1, Math.min(1, k * 2)) }
}

function rand(a: number, b: number) {
  return a + Math.random() * (b - a)
}

export class BattlefieldEngine {
  private map: MlMap | null = null
  private canvas: HTMLCanvasElement | null = null
  private fog: HTMLCanvasElement | null = null
  private raf = 0
  private lastTick = 0
  private lastVeh = 0
  private lastDash = 0
  private dashStep = 0
  private vehDirty = true
  private pic: GroundPicture | null = null
  private gap = 2000
  private vehicles = new Map<string, VehTrack>()
  private players = new Map<string, PlayerTrack>()
  private particles: Particle[] = []
  private selected = new Set<number>()
  private hover: Hover | null = null
  private showFog = false
  private showTerritory = true
  private follow = false
  private reduced = false
  private detachFns: (() => void)[] = []
  /** Called when the viewer drags the map while following their aircraft. */
  private onFollowBroken: () => void = () => {}

  setFollowBroken(fn: () => void) {
    this.onFollowBroken = fn
  }

  setReducedMotion(r: boolean) {
    this.reduced = r
  }

  attach(map: MlMap) {
    this.detach()
    this.map = map
    const cv = document.createElement('canvas')
    cv.className = 'gw-overlay'
    map.getCanvas().after(cv)
    this.canvas = cv
    this.fog = document.createElement('canvas')
    this.resize()
    const onResize = () => this.resize()
    const onRender = () => this.draw()
    const onDrag = () => {
      if (this.follow) this.onFollowBroken()
    }
    const onZoom = () => {
      this.vehDirty = true
    }
    map.on('resize', onResize)
    map.on('render', onRender)
    map.on('dragstart', onDrag)
    map.on('zoomend', onZoom)
    // A new map style drops the vehicle source's data; push it again.
    map.on('styledata', onZoom)
    this.detachFns.push(() => {
      map.off('styledata', onZoom)
      map.off('resize', onResize)
      map.off('render', onRender)
      map.off('dragstart', onDrag)
      map.off('zoomend', onZoom)
    })
    // Players already known (the map was remounted for a style change).
    const old = [...this.players.values()]
    this.players.clear()
    for (const p of old) p.marker.remove()
    if (this.pic) this.setPicture(this.pic, this.gap)
    this.vehDirty = true
    this.loop()
  }

  detach() {
    cancelAnimationFrame(this.raf)
    for (const f of this.detachFns) f()
    this.detachFns = []
    this.canvas?.remove()
    this.canvas = null
    for (const p of this.players.values()) p.marker.remove()
    this.players.clear()
    this.map = null
  }

  destroy() {
    this.detach()
  }

  private resize() {
    const map = this.map
    const cv = this.canvas
    if (!map || !cv || !this.fog) return
    const el = map.getContainer()
    const dpr = window.devicePixelRatio || 1
    cv.width = Math.round(el.clientWidth * dpr)
    cv.height = Math.round(el.clientHeight * dpr)
    this.fog.width = cv.width
    this.fog.height = cv.height
  }

  setSelection(sel: Set<number>) {
    this.selected = sel
    this.vehDirty = true
    for (const v of this.vehicles.values()) v.sel = !v.enemy && sel.has(v.fid)
  }

  setHover(h: Hover | null) {
    this.hover = h
  }

  setOptions(o: { fog: boolean; territory: boolean; follow: boolean }) {
    this.showFog = o.fog
    this.showTerritory = o.territory
    this.follow = o.follow
    this.map?.triggerRepaint()
  }

  setPicture(pic: GroundPicture, gap: number) {
    this.pic = pic
    this.gap = gap
    const t = now()
    const dur = Math.max(600, Math.min(4000, gap))
    const side = pic.side as Side
    const enemySide = other(side)

    // Vehicles.
    const seen = new Set<string>()
    const put = (key: string, pos: LatLon, hdg: number, icon: string, fid: number, op: number, enemy: boolean, moving: boolean) => {
      seen.add(key)
      const cur = this.vehicles.get(key)
      if (cur && cur.icon === icon && dist(cur.to, pos) < 1500) {
        const at = trackPos(cur, t)
        // A moving vehicle is aimed one frame ahead along the way it has been
        // going, so the column stays under its formation's marker instead of
        // trailing a frame behind it.
        const was = cur.real ?? cur.to
        const step: LatLon = [pos[0] - was[0], pos[1] - was[1]]
        const ahead: LatLon = moving && dist(was, pos) < 400 ? [pos[0] + step[0], pos[1] + step[1]] : pos
        Object.assign(cur, { from: at.pos, h0: at.hdg, to: ahead, real: pos, h1: hdg, t0: t, dur, op, fid })
      } else {
        this.vehicles.set(key, { from: pos, to: pos, h0: hdg, h1: hdg, t0: t, dur, icon, fid, op, enemy, sel: false })
      }
      const v = this.vehicles.get(key)
      if (v) v.sel = !enemy && this.selected.has(fid)
    }
    for (const f of pic.formations) {
      f.units.forEach((u, i) =>
        put(`f${f.id}:${i}`, u.pos, u.heading, vehicleImageId(u.role, side, f.broken), f.id, f.live ? 1 : 0.6, false, f.speed_kph > 0),
      )
    }
    for (const e of pic.enemy) {
      if (e.last_seen_secs > 0) continue
      e.units.forEach((u, i) => put(`e${e.id}:${i}`, u.pos, u.heading, vehicleImageId(u.role, enemySide, false), e.id, 1, true, e.moving))
    }
    for (const k of [...this.vehicles.keys()]) if (!seen.has(k)) this.vehicles.delete(k)
    this.vehDirty = true

    // Players.
    const map = this.map
    if (!map) return
    const pseen = new Set<string>()
    for (const p of pic.players) {
      pseen.add(p.name)
      const mps = p.in_air || p.category !== 'plane' ? p.speed_kts * 0.5144 : 0
      // Aim where it will be when the next picture lands, so the marker is
      // never a frame behind the aircraft.
      const ahead = mps > 1.5 ? offset(p.pos, p.heading, mps * (dur / 1000)) : p.pos
      const label = `${p.name}|${p.is_self}|${Math.round((p.alt_m * 3.281) / 100)}|${Math.round(p.speed_kts / 5)}|${p.category}`
      let tr = this.players.get(p.name)
      if (!tr) {
        const el = document.createElement('div')
        el.className = 'gw-pl gw-mk'
        const ico = document.createElement('div')
        ico.className = 'gw-pl-ico'
        const tag = document.createElement('div')
        tag.className = 'gw-pl-tag'
        el.append(ico, tag)
        const marker = new maplibregl.Marker({ element: el, anchor: 'center' }).setLngLat([p.pos[1], p.pos[0]]).addTo(map)
        tr = { marker, el, ico, tag, from: p.pos, to: ahead, h0: p.heading, h1: p.heading, t0: t, dur, mps, cat: p.category, self: p.is_self, label: '' }
        this.players.set(p.name, tr)
      } else {
        const at = this.playerPos(tr, t)
        Object.assign(tr, { from: at.pos, h0: at.hdg, to: ahead, h1: p.heading, t0: t, dur, mps })
      }
      if (tr.label !== label) {
        tr.label = label
        tr.self = p.is_self
        tr.cat = p.category
        tr.el.classList.toggle('self', p.is_self)
        const fill = p.is_self ? PENCIL : SIDE_BRIGHT[side]
        tr.ico.innerHTML = playerSvg(p.category, fill)
        const alt = p.category === 'ground' || p.category === 'ship' ? '' : `${Math.round(p.alt_m * 3.281).toLocaleString()} FT · `
        tr.tag.innerHTML = ''
        const nm = document.createElement('b')
        nm.textContent = p.is_self ? `YOU · ${p.name}` : p.name
        const sub = document.createElement('span')
        sub.textContent = `${p.typ} · ${alt}${Math.round(p.speed_kts)} KT`
        tr.tag.append(nm, sub)
      }
    }
    for (const [k, tr] of [...this.players]) {
      if (!pseen.has(k)) {
        tr.marker.remove()
        this.players.delete(k)
      }
    }
  }

  /** Where the viewer's own unit is drawn right now. */
  selfPos(): LatLon | null {
    for (const tr of this.players.values()) if (tr.self) return this.playerPos(tr, now()).pos
    return null
  }

  private playerPos(tr: PlayerTrack, t: number): { pos: LatLon; hdg: number } {
    const end = tr.t0 + tr.dur
    if (t <= end || tr.mps < 1.5) return trackPos(tr, t)
    // Late picture: keep flying the last heading for a few seconds.
    const over = Math.min(4000, t - end) / 1000
    return { pos: offset(tr.to, tr.h1, tr.mps * over), hdg: tr.h1 }
  }

  private loop = () => {
    this.raf = requestAnimationFrame(this.loop)
    const map = this.map
    if (!map) return
    const t = now()
    // Vehicles: ten updates a second while any are visible and moving.
    const near = map.getZoom() >= NEAR_ZOOM - 0.6
    if (near && (this.vehDirty || t - this.lastVeh > 100)) {
      const moving = this.vehDirty || [...this.vehicles.values()].some((v) => t - v.t0 < v.dur + 120)
      if (moving) this.pushVehicles(t)
    }
    // Route dashes flow toward the destination.
    if (!this.reduced && t - this.lastDash > 70 && map.getLayer(PATH_FLOW_LAYER)) {
      this.lastDash = t
      this.dashStep = (this.dashStep + 1) % DASH_SEQ.length
      map.setPaintProperty(PATH_FLOW_LAYER, 'line-dasharray', DASH_SEQ[this.dashStep])
    }
    if (this.follow) {
      const p = this.selfPos()
      if (p) map.jumpTo({ center: [p[1], p[0]] })
    }
    // Players and effects animate every frame; the map repaints so the
    // overlay is drawn in step with it.
    if (this.players.size || this.particles.length || this.pic?.battles.length || this.hover) {
      if (!this.reduced || t - this.lastTick > 250 || this.players.size) map.triggerRepaint()
    }
  }

  private pushVehicles(t: number) {
    const map = this.map
    if (!map) return
    const src = map.getSource(VEH_SOURCE) as GeoJSONSource | undefined
    if (!src) return
    this.lastVeh = t
    this.vehDirty = false
    const features: Feature<Point>[] = []
    for (const v of this.vehicles.values()) {
      const at = trackPos(v, t)
      features.push({
        type: 'Feature',
        properties: { icon: v.icon, rot: at.hdg, sel: v.sel ? 1 : 0, op: v.op, enemy: v.enemy ? 1 : 0 },
        geometry: { type: 'Point', coordinates: [at.pos[1], at.pos[0]] },
      })
    }
    src.setData({ type: 'FeatureCollection', features })
  }

  // ── Overlay ────────────────────────────────────────────────────────────

  private draw() {
    const map = this.map
    const cv = this.canvas
    const pic = this.pic
    if (!map || !cv) return
    const t = now()
    const dt = Math.min(0.1, (t - (this.lastTick || t)) / 1000)
    this.lastTick = t

    // Players move every frame.
    for (const tr of this.players.values()) {
      const at = this.playerPos(tr, t)
      tr.marker.setLngLat([at.pos[1], at.pos[0]])
      tr.ico.style.transform = `rotate(${at.hdg - map.getBearing()}deg)`
    }

    const c = cv.getContext('2d')
    if (!c) return
    const dpr = window.devicePixelRatio || 1
    c.setTransform(1, 0, 0, 1, 0, 0)
    c.clearRect(0, 0, cv.width, cv.height)
    if (!pic) return
    c.setTransform(dpr, 0, 0, dpr, 0, 0)
    const W = cv.width / dpr
    const H = cv.height / dpr
    const ctr = map.getCenter()
    const p0 = map.project([ctr.lng, ctr.lat])
    const pN = map.project([ctr.lng, ctr.lat + 0.01])
    const pxPerM = Math.abs(p0.y - pN.y) / 1105.74
    const P = (p: LatLon) => map.project([p[1], p[0]])
    const onScreen = (x: number, y: number, pad: number) => x > -pad && y > -pad && x < W + pad && y < H + pad
    const side = pic.side as Side
    const ours = SIDE_BRIGHT[side]
    const theirs = SIDE_BRIGHT[other(side)]

    // Territory: a faint wash of each base's owner.
    if (this.showTerritory) {
      for (const o of pic.objectives) {
        const q = P(o.pos)
        const r = Math.max(36, 7000 * pxPerM)
        if (!onScreen(q.x, q.y, r)) continue
        const g = c.createRadialGradient(q.x, q.y, 0, q.x, q.y, r)
        const col = SIDE_COLOR[o.owner]
        g.addColorStop(0, hexA(col, 0.16))
        g.addColorStop(1, hexA(col, 0))
        c.fillStyle = g
        c.beginPath()
        c.arc(q.x, q.y, r, 0, Math.PI * 2)
        c.fill()
      }
    }

    // Fog of war: dark everywhere our forces can't see.
    if (this.showFog && this.fog) {
      const f = this.fog.getContext('2d')
      if (f) {
        f.setTransform(1, 0, 0, 1, 0, 0)
        f.clearRect(0, 0, this.fog.width, this.fog.height)
        f.setTransform(dpr, 0, 0, dpr, 0, 0)
        f.globalCompositeOperation = 'source-over'
        f.fillStyle = 'rgba(4,6,4,0.58)'
        f.fillRect(0, 0, W, H)
        f.globalCompositeOperation = 'destination-out'
        const hole = (p: LatLon, m: number) => {
          const q = P(p)
          const r = Math.max(10, m * pxPerM)
          if (!onScreen(q.x, q.y, r)) return
          const g = f.createRadialGradient(q.x, q.y, r * 0.7, q.x, q.y, r)
          g.addColorStop(0, 'rgba(0,0,0,1)')
          g.addColorStop(1, 'rgba(0,0,0,0)')
          f.fillStyle = g
          f.beginPath()
          f.arc(q.x, q.y, r, 0, Math.PI * 2)
          f.fill()
        }
        for (const fm of pic.formations) hole(fm.pos, pic.spot_m)
        for (const o of pic.objectives) if (o.owner === side) hole(o.pos, pic.spot_m * 0.6)
        for (const p of pic.players) hole(p.pos, pic.spot_m * 0.5)
        f.globalCompositeOperation = 'source-over'
        c.setTransform(1, 0, 0, 1, 0, 0)
        c.drawImage(this.fog, 0, 0)
        c.setTransform(dpr, 0, 0, dpr, 0, 0)
      }
    }

    // Range rings around what is selected.
    c.save()
    c.setLineDash([3, 5])
    c.lineWidth = 1
    for (const fm of pic.formations) {
      if (!this.selected.has(fm.id)) continue
      const q = P(fm.pos)
      const r = pic.engage_m * pxPerM
      if (r < 12) continue
      c.strokeStyle = hexA(PENCIL, 0.45)
      c.beginPath()
      c.arc(q.x, q.y, r, 0, Math.PI * 2)
      c.stroke()
    }
    c.restore()

    // Battles.
    for (const b of pic.battles) {
      const q = P(b.pos)
      const r = Math.max(16, b.radius_m * pxPerM)
      if (!onScreen(q.x, q.y, r + 40)) continue
      const I = Math.max(0.1, Math.min(1, b.intensity))
      if (!b.live) {
        c.save()
        c.setLineDash([4, 4])
        c.strokeStyle = 'rgba(200,200,190,0.35)'
        c.lineWidth = 1.2
        c.beginPath()
        c.arc(q.x, q.y, r, 0, Math.PI * 2)
        c.stroke()
        c.restore()
        continue
      }
      const flick = this.reduced ? 1 : 0.85 + 0.15 * Math.sin(t / 90 + b.id) * Math.sin(t / 37 + b.id * 3)
      const g = c.createRadialGradient(q.x, q.y, 0, q.x, q.y, r)
      // The glow is for spotting a fight from afar; up close it would tint the whole screen.
      const glow = I * flick * Math.min(1, 160 / r)
      g.addColorStop(0, hexA(FIRE, 0.26 * glow))
      g.addColorStop(0.6, hexA(FIRE, 0.09 * glow))
      g.addColorStop(1, hexA(FIRE, 0))
      c.fillStyle = g
      c.beginPath()
      c.arc(q.x, q.y, r, 0, Math.PI * 2)
      c.fill()
      c.save()
      c.setLineDash([2, 5])
      c.strokeStyle = hexA(FIRE, 0.55)
      c.lineWidth = 1.2
      c.beginPath()
      c.arc(q.x, q.y, r, 0, Math.PI * 2)
      c.stroke()
      c.restore()
      if (!this.reduced) {
        for (let k = 0; k < 2; k++) {
          const ph = (t / 2200 + b.id * 0.37 + k * 0.5) % 1
          c.strokeStyle = hexA(FIRE, (1 - ph) * 0.5 * (0.4 + I * 0.6))
          c.lineWidth = 2 * (1 - ph) + 0.5
          c.beginPath()
          c.arc(q.x, q.y, r * (0.35 + ph * 0.75), 0, Math.PI * 2)
          c.stroke()
        }
        this.spawnBattle(b.id, dt, I, pxPerM)
      }
    }

    // Fire lines between our engaged formations and what they can see.
    const spotted = pic.enemy.filter((e) => e.last_seen_secs === 0)
    for (const fm of pic.formations) {
      if (!fm.engaged) continue
      for (const e of spotted) {
        const d = dist(fm.pos, e.pos)
        if (d > pic.engage_m * 1.25) continue
        const a = P(fm.pos)
        const z = P(e.pos)
        if (!onScreen(a.x, a.y, 200) && !onScreen(z.x, z.y, 200)) continue
        c.lineWidth = 1
        c.strokeStyle = hexA(FIRE, 0.28)
        c.beginPath()
        c.moveTo(a.x, a.y)
        c.lineTo(z.x, z.y)
        c.stroke()
        if (this.reduced) continue
        const len = Math.hypot(z.x - a.x, z.y - a.y)
        const seg = Math.max(5, len * 0.07)
        const tracer = (from: { x: number; y: number }, to: { x: number; y: number }, col: string, seed: number) => {
          for (let k = 0; k < 3; k++) {
            const ph = (t / 650 + seed + k / 3) % 1
            const s = ph
            const e2 = Math.min(1, ph + seg / Math.max(1, len))
            c.strokeStyle = col
            c.lineWidth = 1.6
            c.beginPath()
            c.moveTo(from.x + (to.x - from.x) * s, from.y + (to.y - from.y) * s)
            c.lineTo(from.x + (to.x - from.x) * e2, from.y + (to.y - from.y) * e2)
            c.stroke()
          }
        }
        tracer(a, z, hexA(ours, 0.95), fm.id * 0.13)
        tracer(z, a, hexA(theirs, 0.9), e.id * 0.29 + 0.5)
      }
    }

    // Particles: smoke under, fire over.
    this.particles = this.particles.filter((p) => t - p.born < p.life)
    const wind = { e: 3.5, n: 1.2 }
    for (const p of this.particles) {
      if (p.kind !== 'scorch') continue
      const k = (t - p.born) / p.life
      const q = P(p.pos)
      const r = Math.max(p.minPx, p.m * pxPerM)
      if (!onScreen(q.x, q.y, r)) continue
      const g = c.createRadialGradient(q.x, q.y, 0, q.x, q.y, r)
      g.addColorStop(0, `rgba(12,9,6,${0.55 * (1 - k)})`)
      g.addColorStop(0.7, `rgba(25,18,10,${0.3 * (1 - k)})`)
      g.addColorStop(1, 'rgba(25,18,10,0)')
      c.fillStyle = g
      c.beginPath()
      c.arc(q.x, q.y, r, 0, Math.PI * 2)
      c.fill()
    }
    for (const p of this.particles) {
      if (p.kind !== 'smoke') continue
      const k = (t - p.born) / p.life
      const pos = offsetEN(p.pos, wind.e * ((t - p.born) / 1000), wind.n * ((t - p.born) / 1000))
      const q = P(pos)
      const r = Math.max(p.minPx * (1 + k * 2), p.m * (1 + k * 2.5) * pxPerM)
      if (!onScreen(q.x, q.y, r)) continue
      const g = c.createRadialGradient(q.x, q.y, 0, q.x, q.y, r)
      const a = 0.55 * (k < 0.12 ? k / 0.12 : 1 - (k - 0.12) / 0.88)
      g.addColorStop(0, `rgba(178,172,160,${a})`)
      g.addColorStop(0.5, `rgba(150,145,135,${a * 0.6})`)
      g.addColorStop(1, 'rgba(130,126,118,0)')
      c.fillStyle = g
      c.beginPath()
      c.arc(q.x, q.y, r, 0, Math.PI * 2)
      c.fill()
    }
    c.save()
    c.globalCompositeOperation = 'lighter'
    for (const p of this.particles) {
      if (p.kind === 'smoke' || p.kind === 'scorch') continue
      const k = (t - p.born) / p.life
      const q = P(p.pos)
      if (!onScreen(q.x, q.y, 30)) continue
      if (p.kind === 'flash') {
        const r = Math.max(p.minPx, p.m * pxPerM) * (1.2 - k * 0.4)
        const g = c.createRadialGradient(q.x, q.y, 0, q.x, q.y, r)
        g.addColorStop(0, `rgba(255,255,235,${1 - k})`)
        g.addColorStop(0.4, `rgba(255,214,120,${0.8 * (1 - k)})`)
        g.addColorStop(1, 'rgba(255,160,40,0)')
        c.fillStyle = g
        c.beginPath()
        c.arc(q.x, q.y, r, 0, Math.PI * 2)
        c.fill()
      } else {
        const r = Math.max(p.minPx * (0.6 + k), p.m * (0.4 + k) * pxPerM)
        const g = c.createRadialGradient(q.x, q.y, 0, q.x, q.y, r)
        g.addColorStop(0, `rgba(255,240,200,${Math.max(0, 1 - k * 2.2)})`)
        g.addColorStop(0.35, `rgba(255,140,30,${0.85 * (1 - k)})`)
        g.addColorStop(1, 'rgba(120,30,0,0)')
        c.fillStyle = g
        c.beginPath()
        c.arc(q.x, q.y, r, 0, Math.PI * 2)
        c.fill()
      }
    }
    c.restore()

    // Order preview: grease-pencil lines from every selected formation to the
    // base the cursor has snapped to.
    const h = this.hover
    if (h && h.obj != null && h.verb) {
      const o = pic.objectives.find((x) => x.id === h.obj)
      if (o) {
        const z = P(o.pos)
        const col = VERB_COLOR[h.verb]
        c.save()
        c.strokeStyle = col
        c.fillStyle = col
        c.lineWidth = 2
        c.shadowColor = 'rgba(0,0,0,0.8)'
        c.shadowBlur = 3
        c.setLineDash([9, 6])
        c.lineDashOffset = this.reduced ? 0 : -((t / 30) % 15)
        const from = pic.formations.filter((f) => this.selected.has(f.id))
        for (const f of from) {
          const a = P(f.pos)
          c.beginPath()
          c.moveTo(a.x, a.y)
          c.lineTo(z.x, z.y)
          c.stroke()
          const ang = Math.atan2(z.y - a.y, z.x - a.x)
          const back = 20
          const tip = { x: z.x - Math.cos(ang) * back, y: z.y - Math.sin(ang) * back }
          c.save()
          c.setLineDash([])
          c.beginPath()
          c.moveTo(tip.x, tip.y)
          c.lineTo(tip.x - Math.cos(ang - 0.45) * 11, tip.y - Math.sin(ang - 0.45) * 11)
          c.lineTo(tip.x - Math.cos(ang + 0.45) * 11, tip.y - Math.sin(ang + 0.45) * 11)
          c.closePath()
          c.fill()
          c.restore()
        }
        c.setLineDash([])
        const pulse = this.reduced ? 0 : (t / 900) % 1
        c.lineWidth = 2
        c.beginPath()
        c.arc(z.x, z.y, 18 + pulse * 6, 0, Math.PI * 2)
        c.stroke()
        c.restore()
      }
    }
  }

  private spawnBattle(id: number, dt: number, I: number, pxPerM: number) {
    const pic = this.pic
    if (!pic || this.particles.length > 900) return
    const b = pic.battles.find((x) => x.id === id)
    if (!b) return
    const t = now()
    // Shooters: our vehicles in the fight, and the enemy's we can see.
    const ourUnits: LatLon[] = []
    for (const f of pic.formations) {
      if (!b.ours.includes(f.id)) continue
      for (const u of f.units) ourUnits.push(u.pos)
      if (!f.units.length) ourUnits.push(f.pos)
    }
    const theirUnits: LatLon[] = []
    for (const e of pic.enemy) {
      if (e.last_seen_secs > 0 || dist(e.pos, b.pos) > b.radius_m * 1.6) continue
      for (const u of e.units) theirUnits.push(u.pos)
      if (!e.units.length) theirUnits.push(e.pos)
    }
    const area = (): LatLon => offset(b.pos, rand(0, 360), Math.sqrt(Math.random()) * b.radius_m * 0.55)
    const pick = (arr: LatLon[]): LatLon => (arr.length ? arr[Math.floor(Math.random() * arr.length)] : area())
    const near = pxPerM > 0.02 // close enough that a single vehicle is visible
    const add = (kind: Particle['kind'], pos: LatLon, life: number, m: number, minPx: number) =>
      this.particles.push({ kind, pos, born: t - Math.random() * 30, life, m, minPx })
    const n = (rate: number) => {
      const x = rate * dt
      return Math.floor(x) + (Math.random() < x % 1 ? 1 : 0)
    }
    for (let i = n(12 + 60 * I); i > 0; i--) {
      const fromOurs = Math.random() < 0.55
      const shooter = pick(fromOurs ? ourUnits : theirUnits)
      add('flash', near ? offset(shooter, rand(0, 360), rand(0, 8)) : offset(shooter, rand(0, 360), rand(0, 300)), rand(60, 170), 14, near ? 3.5 : 2.2)
    }
    for (let i = n(1 + 5 * I); i > 0; i--) {
      const target = pick(Math.random() < 0.5 ? ourUnits : theirUnits)
      const at = offset(target, rand(0, 360), rand(10, 80))
      add('boom', at, rand(600, 1100), rand(25, 55), near ? 8 : 4)
      if (Math.random() < 0.7) add('smoke', at, rand(6000, 11000), rand(18, 35), 5)
      // Shell holes stay on the ground a while after the smoke has gone.
      if (Math.random() < 0.6) add('scorch', at, rand(40_000, 70_000), rand(10, 22), 2)
    }
    for (let i = n(0.6 + 1.2 * I); i > 0; i--) add('smoke', area(), rand(8000, 12000), rand(25, 45), 6)
  }
}

function hexA(hex: string, a: number): string {
  const n = parseInt(hex.slice(1), 16)
  return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${Math.max(0, Math.min(1, a)).toFixed(3)})`
}

