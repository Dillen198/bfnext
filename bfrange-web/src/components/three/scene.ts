/**
 * The 3D debrief scene: one three.js world per clip, in metres east / north
 * / up around the pilot (lib/clipPlay.ts). Ours from the ground up: the
 * models are built to size (models.ts, carrier.ts), the ground is the sea
 * for a carrier pass or Esri imagery draped flat at the targets' height
 * over land, and the cameras are the ones a debrief needs:
 *
 *  - chase: behind and above the jet, turning with its heading
 *  - cockpit: the pilot's eye, rolling with the jet
 *  - lso: on the LSO platform, watching the jet come down the groove
 *  - deck: high off the boat's port quarter, the whole flight deck in view
 *  - target: on the ground at the target, looking up at the run
 *  - free: orbit around the jet with the mouse
 *
 * Far away, small things are drawn bigger than life (never smaller than a
 * few pixels) so a bomb or a bandit stays visible; the pilot's own jet and
 * the boat are always true size.
 */
import * as THREE from 'three'
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js'
import { deckFor } from '../../lib/carrierDeck'
import { frameFor, passRead, stateAt, type Frame, type PassRead, type State } from '../../lib/clipPlay'
import type { Clip, ClipObj } from '../../types'
import { carrier, deckSpots } from './carrier'
import { MATS, aircraft, groundUnit, ship, weapon } from './models'

export type CamMode = 'chase' | 'cockpit' | 'lso' | 'deck' | 'target' | 'free'

export interface SceneOpts {
  trails: boolean
  labels: boolean
  glide: boolean
  night: boolean
}

export interface Readout {
  t: number
  self: State | null
  pass: PassRead | null
  /** seconds to the pilot's weapon hitting, while one is falling */
  tti: number | null
}

const D2R = Math.PI / 180
const NM = 1852
const IMAGERY = (z: number, x: number, y: number) => `https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}`

interface Item {
  o: ClipObj
  node: THREE.Object3D
  trail: THREE.Line | null
  trailPts: number
  label: HTMLDivElement | null
  length: number
  minPx: number
}

const ROLE_COLOR: Record<ClipObj['role'], string> = {
  self: '#ffb23e', weapon: '#ffd27a', threat: '#ff5147', boat: '#e4ebf2', air: '#5aa9ff', ground: '#c9ab6f',
}

function skinFor(o: ClipObj): THREE.Material {
  if (o.role === 'self') return MATS.self
  if (o.role === 'weapon') return MATS.weapon
  if (o.role === 'threat') return MATS.threat
  return /red/i.test(o.color ?? '') ? MATS.red : MATS.blue
}

function modelFor(o: ClipObj): THREE.Object3D {
  switch (o.kind) {
    case 'air': return aircraft(o.name, false, skinFor(o))
    case 'helo': return aircraft(o.name, true, skinFor(o))
    case 'missile': case 'bomb': case 'rocket': case 'torpedo': return weapon(o.name, o.kind, skinFor(o))
    case 'carrier': return carrier(deckFor(o.name), o.name)
    case 'ship': return ship(o.name)
    default: return groundUnit(o.kind)
  }
}

/** Esri imagery tiles at zoom `z`, flat at `h`, covering a square of `half` metres around `c`. */
function imagery(f: Frame, c: [number, number], half: number, h: number, z: number): THREE.Group {
  const g = new THREE.Group()
  const lat = f.lat0 + c[1] / 111_320
  const lon = f.lon0 + c[0] / (111_320 * f.k)
  const n = 2 ** z
  const tx = (lo: number) => ((lo + 180) / 360) * n
  const ty = (la: number) => ((1 - Math.log(Math.tan(la * D2R) + 1 / Math.cos(la * D2R)) / Math.PI) / 2) * n
  const lonOf = (x: number) => (x / n) * 360 - 180
  const latOf = (y: number) => { const m = Math.PI - (2 * Math.PI * y) / n; return (180 / Math.PI) * Math.atan(0.5 * (Math.exp(m) - Math.exp(-m))) }
  const dLat = half / 111_320, dLon = half / (111_320 * f.k)
  const x0 = Math.floor(tx(lon - dLon)), x1 = Math.floor(tx(lon + dLon))
  const y0 = Math.floor(ty(lat + dLat)), y1 = Math.floor(ty(lat - dLat))
  const loader = new THREE.TextureLoader()
  loader.setCrossOrigin('anonymous')
  for (let x = x0; x <= x1; x++) {
    for (let y = y0; y <= y1; y++) {
      const [w, nn] = f.toLocal(latOf(y), lonOf(x), h)
      const [e, s] = f.toLocal(latOf(y + 1), lonOf(x + 1), h)
      const mat = new THREE.MeshBasicMaterial({ color: '#3d4436' })
      loader.load(IMAGERY(z, x, y), t => { t.colorSpace = THREE.SRGBColorSpace; mat.map = t; mat.color.set('#ffffff'); mat.needsUpdate = true }, undefined, () => { /* keep the plain colour */ })
      const m = new THREE.Mesh(new THREE.PlaneGeometry(e - w, nn - s), mat)
      m.position.set((w + e) / 2, (nn + s) / 2, h)
      g.add(m)
    }
  }
  return g
}

export class DebriefScene {
  readonly clip: Clip
  readonly frame: Frame
  private renderer: THREE.WebGLRenderer
  private scene = new THREE.Scene()
  private camera: THREE.PerspectiveCamera
  private controls: OrbitControls
  private items: Item[] = []
  private self: Item | null = null
  private boat: Item | null = null
  private target: [number, number, number] | null = null
  private glide: THREE.Group | null = null
  private labelsEl: HTMLDivElement
  private el: HTMLElement
  private mode: CamMode = 'chase'
  private opts: SceneOpts
  private chaseYaw: number | null = null
  private lastT = 0
  private ro: ResizeObserver
  private tmpE = new THREE.Euler(0, 0, 0, 'ZXY')

  constructor(el: HTMLElement, clip: Clip, opts: SceneOpts) {
    this.el = el
    this.clip = clip
    this.opts = opts
    this.frame = frameFor(clip)
    this.renderer = new THREE.WebGLRenderer({ antialias: true, logarithmicDepthBuffer: true })
    this.renderer.setPixelRatio(Math.min(2, window.devicePixelRatio))
    this.renderer.outputColorSpace = THREE.SRGBColorSpace
    el.appendChild(this.renderer.domElement)
    this.labelsEl = document.createElement('div')
    this.labelsEl.className = 'd3-labels'
    el.appendChild(this.labelsEl)

    this.camera = new THREE.PerspectiveCamera(55, 1, 0.3, 200_000)
    this.camera.up.set(0, 0, 1)
    this.controls = new OrbitControls(this.camera, this.renderer.domElement)
    this.controls.enableDamping = true
    this.controls.enabled = false

    this.build()
    this.ro = new ResizeObserver(() => this.resize())
    this.ro.observe(el)
    this.resize()
  }

  private build() {
    const sky = new THREE.Color(this.opts.night ? '#0a1220' : '#9fbfdd')
    this.scene.background = sky
    this.scene.fog = new THREE.Fog(sky, this.opts.night ? 3000 : 8000, this.opts.night ? 40_000 : 90_000)
    this.scene.add(new THREE.HemisphereLight(this.opts.night ? '#334' : '#dfeaf5', this.opts.night ? '#111' : '#4a5040', this.opts.night ? 0.6 : 1.6))
    const sun = new THREE.DirectionalLight('#ffffff', this.opts.night ? 0.25 : 1.6)
    sun.position.set(-3000, -5000, 8000)
    this.scene.add(sun)

    const f = this.frame
    for (const o of this.clip.objects) {
      const node = modelFor(o)
      this.scene.add(node)
      const it: Item = {
        o, node, trail: null, trailPts: 0, label: null, length: node.userData.length ?? 10,
        minPx: o.role === 'ground' ? 5 : o.kind === 'carrier' || o.kind === 'ship' ? 0 : o.role === 'self' ? 0 : 9,
      }
      if (o.role !== 'ground' && o.kind !== 'carrier' && o.kind !== 'ship') {
        const pos = new Float32Array(o.t.length * 3)
        o.t.forEach((_, i) => { const p = f.toLocal(o.lat[i], o.lon[i], o.alt[i]); pos.set(p, i * 3) })
        const geo = new THREE.BufferGeometry()
        geo.setAttribute('position', new THREE.BufferAttribute(pos, 3))
        geo.setDrawRange(0, 0)
        const line = new THREE.Line(geo, new THREE.LineBasicMaterial({ color: ROLE_COLOR[o.role], transparent: true, opacity: o.role === 'self' ? 0.95 : 0.7 }))
        line.frustumCulled = false
        this.scene.add(line)
        it.trail = line
        // the pilot's whole path, faint, so the part still to come shows
        if (o.role === 'self') {
          const ghost = new THREE.Line(geo.clone(), new THREE.LineDashedMaterial({ color: '#ffb23e', transparent: true, opacity: 0.25, dashSize: 30, gapSize: 30 }))
          ghost.geometry.setDrawRange(0, o.t.length)
          ghost.computeLineDistances()
          ghost.frustumCulled = false
          ghost.userData.ghost = true
          this.scene.add(ghost)
        }
      }
      if (o.role !== 'ground' || this.clip.objects.filter(x => x.role === 'ground').length <= 12) {
        const l = document.createElement('div')
        l.className = `d3-label r-${o.role}`
        l.textContent = o.role === 'self' ? (o.pilot ?? 'You') : o.pilot && o.kind !== 'carrier' && o.kind !== 'ship' ? `${o.pilot} · ${o.name ?? ''}` : (o.name ?? o.kind).replace(/_/g, ' ')
        this.labelsEl.appendChild(l)
        it.label = l
      }
      this.items.push(it)
      if (o.role === 'self') this.self = it
      if (o.kind === 'carrier' && !this.boat) this.boat = it
    }

    // the ground under the action
    const ground = this.clip.objects.filter(o => o.role === 'ground')
    const sea = !!this.boat || this.clip.objects.some(o => o.kind === 'ship')
    const h = ground.length ? [...ground.map(o => o.alt[0])].sort((a, b) => a - b)[Math.floor(ground.length / 2)] : 0
    const base = new THREE.Mesh(
      new THREE.PlaneGeometry(400_000, 400_000),
      new THREE.MeshStandardMaterial({ color: sea && !ground.length ? (this.opts.night ? '#061018' : '#123246') : '#3f4637', roughness: 0.9 }),
    )
    base.position.z = sea && !ground.length ? 0 : h - 0.5
    this.scene.add(base)
    if (ground.length || !sea) {
      const at = ground.length ? f.toLocal(ground[0].lat[0], ground[0].lon[0], h) : f.toLocal(this.clip.objects[0].lat[0], this.clip.objects[0].lon[0], h)
      this.target = [at[0], at[1], h]
      this.scene.add(imagery(f, [at[0], at[1]], 7000, h, 14))
      this.scene.add(imagery(f, [at[0], at[1]], 30_000, h - 0.3, 11))
    }

    // the glideslope: 3.5° from the landing point out to 1.5 NM, with the ±0.8° edges
    if (this.boat) {
      const g = new THREE.Group()
      const gs = deckFor(this.boat.o.name).glideslope_deg
      for (const [d, color, op] of [[0, '#38d77c', 0.9], [0.8, '#e9c43f', 0.45], [-0.8, '#e9c43f', 0.45]] as const) {
        const a = (gs + d) * D2R
        const geo = new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0, 0, 0), new THREE.Vector3(0, -1.5 * NM, Math.tan(a) * 1.5 * NM)])
        g.add(new THREE.Line(geo, new THREE.LineDashedMaterial({ color, transparent: true, opacity: op, dashSize: 20, gapSize: 12 })))
        ;(g.children[g.children.length - 1] as THREE.Line).computeLineDistances()
      }
      this.glide = g
      this.scene.add(g)
    }
  }

  hasBoat() { return !!this.boat }
  hasTarget() { return !!this.target }

  setMode(m: CamMode) {
    this.mode = m
    this.controls.enabled = m === 'free'
    this.chaseYaw = null
    if (m === 'free' && this.self) {
      const p = this.self.node.position
      this.camera.position.set(p.x - 120, p.y - 160, p.z + 70)
      this.controls.target.copy(p)
    }
  }

  setOpts(o: SceneOpts) { this.opts = o }

  private resize() {
    const w = this.el.clientWidth, h = this.el.clientHeight
    if (!w || !h) return
    this.renderer.setSize(w, h, false)
    this.renderer.domElement.style.width = '100%'
    this.renderer.domElement.style.height = '100%'
    this.camera.aspect = w / h
    this.camera.updateProjectionMatrix()
  }

  /** Put everything where it is at `t` and draw. */
  update(t: number, dt: number): Readout {
    const f = this.frame
    let selfSt: State | null = null
    let boatSt: State | null = null
    const camPos = this.camera.position
    const pxPerRad = this.el.clientHeight / (this.camera.fov * D2R)
    for (const it of this.items) {
      const st = stateAt(it.o, f, t)
      it.node.visible = !st.hidden
      it.node.position.set(st.x, st.y, st.z)
      this.tmpE.set(st.pitch * D2R, st.roll * D2R, -st.yaw * D2R, 'ZXY')
      it.node.quaternion.setFromEuler(this.tmpE)
      // spin the rotors
      it.node.traverse(n => { if (n.userData.spin) n.rotation.z += n.userData.spin * dt })
      if (it.minPx > 0) {
        const d = camPos.distanceTo(it.node.position)
        const px = (it.length / Math.max(1, d)) * pxPerRad
        it.node.scale.setScalar(Math.max(1, it.minPx / Math.max(px, 1e-3)))
      }
      if (it.trail) {
        let n = 0
        while (n < it.o.t.length && it.o.t[n] <= t) n++
        it.trail.geometry.setDrawRange(0, this.opts.trails ? n : 0)
        it.trail.visible = this.opts.trails
      }
      if (it === this.self) selfSt = st
      if (it === this.boat) boatSt = st
    }
    this.scene.children.forEach(c => { if (c.userData.ghost) c.visible = this.opts.trails })

    let pass: PassRead | null = null
    if (selfSt && boatSt && this.boat) {
      const g = deckFor(this.boat.o.name)
      pass = passRead(selfSt, boatSt, g)
      if (this.glide) {
        this.glide.visible = this.opts.glide
        this.glide.position.set(...pass.lp)
        this.glide.rotation.set(0, 0, -pass.fb_deg * D2R)
      }
    }

    this.placeCamera(selfSt, boatSt, dt)
    this.renderer.render(this.scene, this.camera)
    this.placeLabels()

    // time to impact of the pilot's weapon still in the air
    let tti: number | null = null
    for (const it of this.items) {
      if (it.o.role !== 'weapon') continue
      const end = it.o.gone ?? it.o.t[it.o.t.length - 1]
      if (t >= it.o.t[0] && t <= end) tti = Math.min(tti ?? Infinity, end - t)
    }
    this.lastT = t
    return { t, self: selfSt, pass, tti }
  }

  private placeCamera(me: State | null, boat: State | null, dt: number) {
    const cam = this.camera
    if (!me) { this.controls.update(); return }
    const pos = new THREE.Vector3(me.x, me.y, me.z)
    const len = this.self?.length ?? 15
    if (this.mode === 'free') {
      // keep orbiting the jet as it moves
      const delta = pos.clone().sub(this.controls.target)
      cam.position.add(delta)
      this.controls.target.copy(pos)
      this.controls.update()
      cam.fov = 55
    } else if (this.mode === 'cockpit') {
      const q = this.self!.node.quaternion
      const eye = new THREE.Vector3(0, len * 0.27, 1.0).applyQuaternion(q).add(pos)
      cam.position.copy(eye)
      cam.up.set(0, 0, 1).applyQuaternion(q)
      cam.lookAt(new THREE.Vector3(0, 100, 0.4).applyQuaternion(q).add(eye))
      cam.fov = 72
    } else if (this.mode === 'lso' && boat && this.boat) {
      const g = deckFor(this.boat.o.name)
      const spot = deckSpots(g).lso ?? [-25, g.stern_m + 60, g.deck_height_m]
      const h = boat.yaw * D2R
      const x = boat.x + Math.cos(h) * spot[0] + Math.sin(h) * spot[1]
      const y = boat.y - Math.sin(h) * spot[0] + Math.cos(h) * spot[1]
      cam.up.set(0, 0, 1)
      cam.position.set(x, y, boat.z + spot[2] + 1.8)
      cam.lookAt(pos)
      // zoom in with the distance, like a pair of binoculars, down to a normal eye
      const d = cam.position.distanceTo(pos)
      cam.fov = Math.max(14, Math.min(60, (Math.atan2(len * 6, d) / D2R) * 2))
    } else if (this.mode === 'deck' && boat && this.boat) {
      const g = deckFor(this.boat.o.name)
      const h = boat.yaw * D2R
      // model offset: to port, aft, up; turned into the world by the boat's heading
      const [ox, oy, oz] = [-g.length_m * 0.55, -g.length_m * 0.75, g.length_m * 0.42]
      cam.up.set(0, 0, 1)
      cam.position.set(boat.x + Math.cos(h) * ox + Math.sin(h) * oy, boat.y - Math.sin(h) * ox + Math.cos(h) * oy, boat.z + oz)
      cam.lookAt(boat.x + Math.sin(h) * g.length_m * 0.05, boat.y + Math.cos(h) * g.length_m * 0.05, boat.z + g.deck_height_m)
      cam.fov = 50
    } else if (this.mode === 'target' && this.target) {
      cam.up.set(0, 0, 1)
      cam.position.set(this.target[0] + 40, this.target[1] - 60, this.target[2] + 6)
      cam.lookAt(pos)
      const d = cam.position.distanceTo(pos)
      cam.fov = Math.max(10, Math.min(65, (Math.atan2(len * 8, d) / D2R) * 2))
    } else {
      // chase: behind and above, the heading smoothed so turns do not whip
      const want = me.yaw
      this.chaseYaw = this.chaseYaw === null ? want : this.chaseYaw + (((want - this.chaseYaw + 540) % 360) - 180) * Math.min(1, dt * 2.5)
      const h = this.chaseYaw * D2R
      const back = len * 3.2, up = len * 0.9
      cam.up.set(0, 0, 1)
      cam.position.set(pos.x - Math.sin(h) * back, pos.y - Math.cos(h) * back, pos.z + up)
      cam.lookAt(pos.x + Math.sin(h) * len, pos.y + Math.cos(h) * len, pos.z)
      cam.fov = 55
    }
    cam.updateProjectionMatrix()
  }

  private placeLabels() {
    const w = this.el.clientWidth, h = this.el.clientHeight
    const v = new THREE.Vector3()
    for (const it of this.items) {
      if (!it.label) continue
      const show = this.opts.labels && it.node.visible && !(this.mode === 'cockpit' && it === this.self)
      if (!show) { it.label.style.display = 'none'; continue }
      v.copy(it.node.position)
      v.z += it.length * 0.4 * it.node.scale.x
      v.project(this.camera)
      if (v.z > 1 || v.z < -1) { it.label.style.display = 'none'; continue }
      it.label.style.display = ''
      it.label.style.transform = `translate(${((v.x + 1) / 2) * w}px, ${((1 - v.y) / 2) * h}px) translate(-50%, -130%)`
    }
  }

  dispose() {
    this.ro.disconnect()
    this.controls.dispose()
    this.scene.traverse(o => {
      const m = o as THREE.Mesh
      m.geometry?.dispose?.()
      const mats = Array.isArray(m.material) ? m.material : m.material ? [m.material] : []
      mats.forEach(x => { if (!Object.values(MATS).includes(x as never)) { (x as THREE.MeshBasicMaterial).map?.dispose(); x.dispose() } })
    })
    this.renderer.dispose()
    this.renderer.domElement.remove()
    this.labelsEl.remove()
  }

  get time() { return this.lastT }
}
