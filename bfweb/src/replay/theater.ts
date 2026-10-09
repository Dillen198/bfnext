// The replay's 3D theatre: a three.js scene of its own (not a map layer), so
// the camera can go anywhere a Tacview camera can -- into the cockpit, behind
// the jet, looking up at a bandit, or framing a shooter and its target.
//
// World space is metres east/north/up of an origin near the action, with the
// earth's curvature folded in (a point d metres away sits d^2/2R lower), so the
// horizon falls away as it should and distances read right. Terrain is
// Mapzen/AWS terrarium elevation tiles draped with Esri imagery, loaded in two
// rings around whatever the camera looks at: a coarse one for the horizon and
// a detailed one under the action.

import * as THREE from 'three'
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js'
import type { Kind, State } from './data'
import { familyOf, modelOf, realOf, realModel, realLength, LENGTH, MIN_PX, type Family, type Real } from './models'

export type Cam3D = 'free' | 'follow' | 'chase' | 'cockpit' | 'padlock'

export interface TheaterObject {
  idx: number
  kind: Kind
  name?: string | null
  color: string
  st: State
  focus: boolean
}

export interface TheaterFrame {
  objects: TheaterObject[]
  trails: { pts: [number, number, number][]; color: string; opacity: number }[]
  /** BRAA line, focus -> measured */
  line: [number, number, number][] | null
  focus: number | null
  measure: number | null
  cam: Cam3D
}

export interface Projected { idx: number; x: number; y: number }

const R_EARTH = 6_371_000
const EARTH_C = 40_075_016.686
const DEG = Math.PI / 180
const FOCUS_HEX = '#b6f04a'
const TERRARIUM = (z: number, x: number, y: number) => `https://s3.amazonaws.com/elevation-tiles-prod/terrarium/${z}/${x}/${y}.png`
const IMAGERY = (z: number, x: number, y: number) => `https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}`

// ── tile maths ─────────────────────────────────────────────────────────

function lon2tx(lon: number, z: number) { return ((lon + 180) / 360) * 2 ** z }
function lat2ty(lat: number, z: number) {
  const r = lat * DEG
  return ((1 - Math.log(Math.tan(r) + 1 / Math.cos(r)) / Math.PI) / 2) * 2 ** z
}
function tx2lon(x: number, z: number) { return (x / 2 ** z) * 360 - 180 }
function ty2lat(y: number, z: number) {
  const n = Math.PI - (2 * Math.PI * y) / 2 ** z
  return (180 / Math.PI) * Math.atan(0.5 * (Math.exp(n) - Math.exp(-n)))
}

function loadImage(url: string): Promise<HTMLImageElement> {
  return new Promise((res, rej) => {
    const img = new Image()
    img.crossOrigin = 'anonymous'
    img.onload = () => res(img)
    img.onerror = () => rej(new Error(`tile ${url}`))
    img.src = url
  })
}

interface Dem { z: number; x: number; y: number; h: Float32Array } // 256x256 metres

async function loadDem(z: number, x: number, y: number): Promise<Dem> {
  const img = await loadImage(TERRARIUM(z, x, y))
  const cv = document.createElement('canvas')
  cv.width = cv.height = 256
  const ctx = cv.getContext('2d', { willReadFrequently: true })!
  ctx.drawImage(img, 0, 0)
  const d = ctx.getImageData(0, 0, 256, 256).data
  const h = new Float32Array(256 * 256)
  for (let i = 0; i < h.length; i++) {
    // terrarium: (R*256 + G + B/256) - 32768. The sea floor is not DCS's
    // sea, which is flat at 0.
    h[i] = Math.max(0, d[i * 4] * 256 + d[i * 4 + 1] + d[i * 4 + 2] / 256 - 32768)
  }
  return { z, x, y, h }
}

function demAt(dem: Dem, lon: number, lat: number): number {
  const fx = (lon2tx(lon, dem.z) - dem.x) * 255
  const fy = (lat2ty(lat, dem.z) - dem.y) * 255
  const x0 = Math.max(0, Math.min(254, Math.floor(fx))), y0 = Math.max(0, Math.min(254, Math.floor(fy)))
  const ax = Math.min(1, Math.max(0, fx - x0)), ay = Math.min(1, Math.max(0, fy - y0))
  const h = dem.h
  const a = h[y0 * 256 + x0], b = h[y0 * 256 + x0 + 1], c = h[(y0 + 1) * 256 + x0], d = h[(y0 + 1) * 256 + x0 + 1]
  return (a * (1 - ax) + b * ax) * (1 - ay) + (c * (1 - ax) + d * ax) * ay
}

// ── the theatre ────────────────────────────────────────────────────────

interface Tile {
  key: string
  z: number; x: number; y: number
  ring: 'base' | 'detail'
  mesh?: THREE.Mesh
  dem?: Dem
  loading: boolean
  used: number
}

export class Theater {
  readonly el: HTMLDivElement
  private renderer: THREE.WebGLRenderer
  private scene = new THREE.Scene()
  readonly camera: THREE.PerspectiveCamera
  private controls: OrbitControls
  private origin: { lon: number; lat: number } | null = null
  private tiles = new Map<string, Tile>()
  private terrain = new THREE.Group()
  private pools = new Map<string, THREE.InstancedMesh>()
  private lines: THREE.Line[] = []
  private drop: THREE.Line
  private braa: THREE.Line
  private frameNo = 0
  private lastFocus: THREE.Vector3 | null = null
  private chaseDist = 120
  private padlockDist = 180
  private tmpM = new THREE.Matrix4()
  private tmpQ = new THREE.Quaternion()
  private tmpE = new THREE.Euler(0, 0, 0, 'ZXY')
  private camFix = new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3(1, 0, 0), Math.PI / 2)
  projected: Projected[] = []
  /** the camera moved by the user (orbit, zoom): a frame is needed */
  onChange: (() => void) | null = null
  /** the user dragged the view while following: hand over to free */
  onUserMove: (() => void) | null = null
  private disposed = false

  constructor(parent: HTMLElement) {
    this.el = document.createElement('div')
    this.el.className = 'rp-theater'
    parent.appendChild(this.el)
    this.renderer = new THREE.WebGLRenderer({ antialias: true, logarithmicDepthBuffer: true })
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2))
    this.el.appendChild(this.renderer.domElement)
    this.camera = new THREE.PerspectiveCamera(55, 1, 1, 2_000_000)
    this.camera.up.set(0, 0, 1)
    this.camera.position.set(0, -20_000, 12_000)
    this.controls = new OrbitControls(this.camera, this.renderer.domElement)
    this.controls.enableDamping = false
    this.controls.maxPolarAngle = Math.PI * 0.98
    this.controls.minDistance = 20
    this.controls.maxDistance = 900_000
    this.controls.zoomSpeed = 1.4
    this.controls.addEventListener('change', () => this.onChange?.())
    this.controls.addEventListener('start', () => this.onUserMove?.())

    const sky = new THREE.Color('#8fb3d6')
    this.scene.background = sky
    this.scene.fog = new THREE.Fog(sky, 60_000, 400_000)
    this.scene.add(new THREE.HemisphereLight(0xdfeeff, 0x4a4030, 1.6))
    const sun = new THREE.DirectionalLight(0xfff4e0, 1.8)
    sun.position.set(0.5, -0.4, 1)
    this.scene.add(sun)
    this.scene.add(this.terrain)

    const mkLine = (color: string, opacity: number) => {
      const g = new THREE.BufferGeometry().setAttribute('position', new THREE.Float32BufferAttribute(new Float32Array(6), 3))
      const l = new THREE.Line(g, new THREE.LineBasicMaterial({ color, transparent: true, opacity, depthTest: false }))
      l.frustumCulled = false
      l.renderOrder = 10
      this.scene.add(l)
      return l
    }
    this.drop = mkLine(FOCUS_HEX, 0.75)
    this.braa = mkLine('#ffffff', 0.95)

    // Chase / padlock distance on the wheel (OrbitControls owns it otherwise).
    this.renderer.domElement.addEventListener('wheel', ev => {
      if (this.controls.enabled) return
      ev.preventDefault()
      const f = Math.exp(ev.deltaY * 0.0012)
      this.chaseDist = Math.min(20_000, Math.max(25, this.chaseDist * f))
      this.padlockDist = Math.min(40_000, Math.max(40, this.padlockDist * f))
      this.onChange?.()
    }, { passive: false })

    const ro = new ResizeObserver(() => this.resize())
    ro.observe(this.el)
    this.resize()
  }

  destroy() {
    this.disposed = true
    this.controls.dispose()
    for (const t of this.tiles.values()) this.disposeTile(t)
    for (const m of this.pools.values()) (m.material as THREE.Material).dispose()
    this.renderer.dispose()
    this.el.remove()
  }

  private resize() {
    const w = this.el.clientWidth, h = this.el.clientHeight
    if (!w || !h) return
    this.renderer.setSize(w, h, false)
    this.camera.aspect = w / h
    this.camera.updateProjectionMatrix()
    this.onChange?.()
  }

  // ── coordinates ──

  /** Pick the world origin (once, near the action). */
  ensureOrigin(lon: number, lat: number) {
    if (this.origin) return
    this.origin = { lon, lat }
  }

  /** lon/lat/alt -> world, curvature included. */
  toWorld(lon: number, lat: number, alt: number, out = new THREE.Vector3()): THREE.Vector3 {
    const o = this.origin!
    const x = (lon - o.lon) * 111_320 * Math.cos(lat * DEG)
    const y = (lat - o.lat) * 110_540
    return out.set(x, y, alt - (x * x + y * y) / (2 * R_EARTH))
  }
  /** world x/y -> lon/lat (inverse of toWorld, ignoring curvature). */
  private toLonLat(x: number, y: number): [number, number] {
    const o = this.origin!
    const lat = o.lat + y / 110_540
    return [o.lon + x / (111_320 * Math.cos(lat * DEG)), lat]
  }

  /** Terrain height (m MSL) under a point, from the finest tile loaded. */
  heightAt(lon: number, lat: number): number | null {
    let best: Dem | null = null
    for (const t of this.tiles.values()) {
      const d = t.dem
      if (!d || (best && d.z <= best.z)) continue
      const tx = lon2tx(lon, d.z), ty = lat2ty(lat, d.z)
      if (tx >= d.x && tx < d.x + 1 && ty >= d.y && ty < d.y + 1) best = d
    }
    return best ? demAt(best, lon, lat) : null
  }

  // ── terrain ──

  private disposeTile(t: Tile) {
    if (t.mesh) {
      this.terrain.remove(t.mesh)
      t.mesh.geometry.dispose()
      const m = t.mesh.material as THREE.MeshLambertMaterial
      m.map?.dispose()
      m.dispose()
    }
  }

  private async loadTile(t: Tile) {
    t.loading = true
    try {
      const dz = Math.min(t.z, 13)
      const k = 2 ** (t.z - dz)
      const dem = await loadDem(dz, Math.floor(t.x / k), Math.floor(t.y / k))
      // Imagery one zoom finer than the mesh, four tiles to one 512 texture.
      const iz = Math.min(t.z + 1, 17)
      const cv = document.createElement('canvas')
      cv.width = cv.height = 512
      const ctx = cv.getContext('2d')!
      await Promise.all([0, 1].flatMap(dy => [0, 1].map(async dx => {
        try {
          const img = await loadImage(IMAGERY(iz, t.x * 2 + dx, t.y * 2 + dy))
          ctx.drawImage(img, dx * 256, dy * 256)
        } catch { /* a missing imagery tile stays grey */ }
      })))
      if (this.disposed || !this.tiles.has(t.key)) return
      const tex = new THREE.CanvasTexture(cv)
      tex.colorSpace = THREE.SRGBColorSpace
      tex.anisotropy = 4
      const N = t.ring === 'detail' ? 48 : 24
      const g = new THREE.PlaneGeometry(1, 1, N, N)
      const pos = g.getAttribute('position') as THREE.BufferAttribute
      const uv = g.getAttribute('uv') as THREE.BufferAttribute
      const v = new THREE.Vector3()
      const sink = t.ring === 'base' ? 60 : 0
      for (let i = 0; i < pos.count; i++) {
        const u = uv.getX(i), w = 1 - uv.getY(i)
        const lon = tx2lon(t.x + u, t.z), lat = ty2lat(t.y + w, t.z)
        this.toWorld(lon, lat, demAt(dem, lon, lat) - sink, v)
        pos.setXYZ(i, v.x, v.y, v.z)
      }
      g.computeVertexNormals()
      const mesh = new THREE.Mesh(g, new THREE.MeshLambertMaterial({ map: tex }))
      mesh.renderOrder = t.ring === 'base' ? -2 : -1
      t.mesh = mesh
      t.dem = { ...dem }
      // A finer DEM than the mesh's own zoom is never requested, so the
      // height lookup works from the tile's DEM directly.
      this.terrain.add(mesh)
      this.onChange?.()
    } catch {
      // leave it to be retried on a later frame
      this.tiles.delete(t.key)
    } finally {
      t.loading = false
    }
  }

  /** Keep both terrain rings centred on what the camera looks at. */
  private updateTerrain(target: THREE.Vector3) {
    if (!this.origin) return
    const camH = Math.max(50, this.camera.position.z - (this.heightAt(...this.toLonLat(this.camera.position.x, this.camera.position.y)) ?? 0))
    const dist = Math.max(camH, this.camera.position.distanceTo(target))
    const [lon, lat] = this.toLonLat(target.x, target.y)
    const span = Math.max(1500, dist * 1.2) // metres one detail tile should cover
    const zd = Math.max(6, Math.min(14, Math.round(Math.log2((EARTH_C * Math.cos(lat * DEG)) / span))))
    const zb = Math.max(4, zd - 4)
    const want = new Set<string>()
    const ring = (z: number, n: number, kind: 'base' | 'detail') => {
      const cx = Math.floor(lon2tx(lon, z)), cy = Math.floor(lat2ty(lat, z))
      const max = 2 ** z
      for (let dy = -n; dy <= n; dy++) {
        for (let dx = -n; dx <= n; dx++) {
          const x = (cx + dx + max) % max, y = cy + dy
          if (y < 0 || y >= max) continue
          const key = `${z}/${x}/${y}`
          want.add(key)
          let t = this.tiles.get(key)
          if (!t) {
            t = { key, z, x, y, ring: kind, loading: false, used: this.frameNo }
            this.tiles.set(key, t)
          }
          t.used = this.frameNo
        }
      }
    }
    ring(zb, 3, 'base')
    ring(zd, 2, 'detail')
    // Start a few loads per frame, nearest first by construction order.
    let started = 0
    for (const t of this.tiles.values()) {
      if (!t.mesh && !t.loading && want.has(t.key) && started < 4) {
        started++
        void this.loadTile(t)
      }
    }
    // Hide tiles not wanted now; drop long-unused ones.
    for (const t of [...this.tiles.values()]) {
      if (t.mesh) t.mesh.visible = want.has(t.key)
      if (!want.has(t.key) && this.frameNo - t.used > 600 && !t.loading) {
        this.disposeTile(t)
        this.tiles.delete(t.key)
      }
    }
  }

  // ── drawing ──

  /** The geometry an object is drawn with: its real model once loaded,
   *  else the built-in shape for its family. */
  private shape(f: Family, real: Real | null): { key: string; geo: THREE.BufferGeometry; length: number } {
    if (real) {
      const g = realModel(real, () => this.onChange?.())
      if (g) return { key: `real:${real}`, geo: g, length: realLength(g) }
    }
    return { key: f, geo: modelOf(f), length: LENGTH[f] }
  }

  private pool(key: string, geo: THREE.BufferGeometry, color: string, need: number): THREE.InstancedMesh {
    const pk = `${key}|${color}`
    let m = this.pools.get(pk)
    if (!m || m.instanceMatrix.count < need) {
      if (m) { this.scene.remove(m); (m.material as THREE.Material).dispose() }
      const real = key.startsWith('real:')
      m = new THREE.InstancedMesh(geo, new THREE.MeshLambertMaterial({ color, side: THREE.DoubleSide, flatShading: real }), Math.max(16, need * 2))
      m.instanceMatrix.setUsage(THREE.DynamicDrawUsage)
      m.frustumCulled = false
      this.pools.set(pk, m)
      this.scene.add(m)
    }
    return m
  }

  private placeCamera(frame: TheaterFrame, focusPos: THREE.Vector3 | null, focusSt: State | null, measurePos: THREE.Vector3 | null) {
    const cam = this.camera
    const follow = frame.cam !== 'free' && focusPos
    this.controls.enabled = !focusPos || frame.cam === 'free' || frame.cam === 'follow'
    if (!follow || !focusPos) {
      this.lastFocus = null
      return
    }
    if (frame.cam === 'follow') {
      // Orbit the aircraft: carry the camera along with it.
      if (this.lastFocus) {
        const d = focusPos.clone().sub(this.lastFocus)
        cam.position.add(d)
      } else {
        cam.position.copy(focusPos).add(new THREE.Vector3(0, -400, 150))
      }
      this.controls.target.copy(focusPos)
      this.lastFocus = focusPos.clone()
      this.controls.update()
      return
    }
    this.lastFocus = null
    if (!focusSt) return
    this.tmpE.set(focusSt.pitch * DEG, focusSt.roll * DEG, -focusSt.hdg * DEG, 'ZXY')
    const att = new THREE.Quaternion().setFromEuler(this.tmpE)
    const fwd = new THREE.Vector3(0, 1, 0).applyQuaternion(att)
    if (frame.cam === 'cockpit') {
      cam.position.copy(focusPos).add(new THREE.Vector3(0, 0, 1.2).applyQuaternion(att)).add(fwd.clone().multiplyScalar(4))
      cam.quaternion.copy(att).multiply(this.camFix)
      return
    }
    if (frame.cam === 'padlock' && measurePos) {
      // Behind the aircraft on the line from the target, both in frame.
      const dir = focusPos.clone().sub(measurePos).normalize()
      cam.position.copy(focusPos).add(dir.multiplyScalar(this.padlockDist)).add(new THREE.Vector3(0, 0, this.padlockDist * 0.18))
      cam.up.set(0, 0, 1)
      cam.lookAt(measurePos.clone().lerp(focusPos, 0.25))
      return
    }
    // chase: behind and a little above, along the flight path, wings level
    const flat = new THREE.Vector3(Math.sin(focusSt.hdg * DEG), Math.cos(focusSt.hdg * DEG), 0)
    cam.position.copy(focusPos).add(flat.multiplyScalar(-this.chaseDist)).add(new THREE.Vector3(0, 0, this.chaseDist * 0.22))
    cam.up.set(0, 0, 1)
    cam.lookAt(focusPos.clone().add(new THREE.Vector3(0, 0, this.chaseDist * 0.05)))
  }

  /** Frame everything given (used once, when nothing is followed). */
  frameOn(points: [number, number, number][]) {
    if (!points.length) return
    this.ensureOrigin(points[0][0], points[0][1])
    const box = new THREE.Box3()
    for (const p of points) box.expandByPoint(this.toWorld(p[0], p[1], p[2]))
    const c = box.getCenter(new THREE.Vector3())
    const r = Math.max(5_000, box.getSize(new THREE.Vector3()).length() * 0.6)
    this.controls.target.copy(c)
    this.camera.position.copy(c).add(new THREE.Vector3(0, -r, r * 0.6))
    this.controls.update()
  }

  render(frame: TheaterFrame) {
    this.frameNo++
    const focusObj = frame.focus != null ? frame.objects.find(o => o.idx === frame.focus) : undefined
    if (!this.origin) {
      const first = focusObj ?? frame.objects[0]
      if (!first) return
      this.ensureOrigin(first.st.lon, first.st.lat)
      if (!focusObj) this.frameOn(frame.objects.filter(o => o.kind === 'air' || o.kind === 'helo').map(o => [o.st.lon, o.st.lat, o.st.alt]))
    }
    const v = new THREE.Vector3()
    const w = this.el.clientWidth, h = this.el.clientHeight

    // Positions first (the camera follows the focus).
    const placed: { o: TheaterObject; p: THREE.Vector3; f: Family; key: string; geo: THREE.BufferGeometry; length: number }[] = []
    let focusPos: THREE.Vector3 | null = null
    let measurePos: THREE.Vector3 | null = null
    for (const o of frame.objects) {
      const f = familyOf(o.kind, o.name)
      const air = o.kind === 'air' || o.kind === 'helo'
      const weapon = f === 'missile' || f === 'bomb'
      let alt = o.st.alt
      const g = this.heightAt(o.st.lon, o.st.lat)
      if (air || weapon) {
        if (g != null && alt < g + 1) alt = g + 1
      } else if (o.kind === 'ship' || o.kind === 'carrier') {
        alt = 0
      } else if (g != null) {
        alt = g
      }
      const p = this.toWorld(o.st.lon, o.st.lat, alt, new THREE.Vector3())
      placed.push({ o, p, f, ...this.shape(f, realOf(o.kind, o.name)) })
      if (o.idx === frame.focus) focusPos = p
      if (o.idx === frame.measure) measurePos = p
    }
    this.placeCamera(frame, focusPos, focusObj?.st ?? null, measurePos)
    this.camera.updateMatrixWorld()
    const target = focusPos ?? this.controls.target
    this.updateTerrain(target)

    // Fog follows height: a long view from high up, a hazy one down low.
    const camAlt = Math.max(100, this.camera.position.z)
    const fog = this.scene.fog as THREE.Fog
    fog.near = Math.max(20_000, camAlt * 6)
    fog.far = Math.max(150_000, camAlt * 40)

    // Instances.
    for (const m of this.pools.values()) m.count = 0
    const buckets = new Map<string, typeof placed>()
    for (const x of placed) {
      const key = `${x.key}|${x.o.focus ? FOCUS_HEX : x.o.color}`
      let b = buckets.get(key)
      if (!b) buckets.set(key, (b = []))
      b.push(x)
    }
    const pxPerRad = h / (2 * Math.tan((this.camera.fov * DEG) / 2))
    const s3 = new THREE.Vector3()
    const projected: Projected[] = []
    for (const [key, list] of buckets) {
      const color = key.slice(key.lastIndexOf('|') + 1)
      const { f, key: shapeKey, geo, length } = list[0]
      const mesh = this.pool(shapeKey, geo, color, list.length)
      let n = 0
      for (const { o, p } of list) {
        const d = this.camera.position.distanceTo(p)
        // In the cockpit, the own aircraft is not drawn over the view.
        if (frame.cam === 'cockpit' && o.focus) continue
        const scale = Math.max(1, (MIN_PX[f] * d) / pxPerRad / length)
        this.tmpE.set(o.st.pitch * DEG, o.st.roll * DEG, -o.st.hdg * DEG, 'ZXY')
        this.tmpQ.setFromEuler(this.tmpE)
        s3.set(scale, scale, scale)
        this.tmpM.compose(p, this.tmpQ, s3)
        mesh.setMatrixAt(n++, this.tmpM)
        if (o.kind === 'air' || o.kind === 'helo' || f === 'missile' || f === 'bomb') {
          v.copy(p).project(this.camera)
          if (v.z < 1 && Math.abs(v.x) <= 1.1 && Math.abs(v.y) <= 1.1) {
            projected.push({ idx: o.idx, x: ((v.x + 1) / 2) * w, y: ((1 - v.y) / 2) * h })
          }
        }
      }
      mesh.count = n
      mesh.instanceMatrix.needsUpdate = true
    }
    this.projected = projected

    // Trails.
    while (this.lines.length < frame.trails.length) {
      const l = new THREE.Line(new THREE.BufferGeometry(), new THREE.LineBasicMaterial({ transparent: true }))
      l.frustumCulled = false
      this.lines.push(l)
      this.scene.add(l)
    }
    this.lines.forEach((l, i) => {
      const tr = frame.trails[i]
      l.visible = !!tr && tr.pts.length > 1
      if (!l.visible) return
      const arr = new Float32Array(tr.pts.length * 3)
      tr.pts.forEach(([lon, lat, alt], k) => {
        this.toWorld(lon, lat, alt, v)
        arr[k * 3] = v.x; arr[k * 3 + 1] = v.y; arr[k * 3 + 2] = v.z
      })
      l.geometry.setAttribute('position', new THREE.BufferAttribute(arr, 3))
      const mat = l.material as THREE.LineBasicMaterial
      mat.color.set(tr.color)
      mat.opacity = tr.opacity
    })

    // Drop line from the followed aircraft to the ground; BRAA line.
    const setLine = (l: THREE.Line, a: THREE.Vector3 | null, b: THREE.Vector3 | null) => {
      l.visible = !!(a && b)
      if (!a || !b) return
      const pos = l.geometry.getAttribute('position') as THREE.BufferAttribute
      pos.setXYZ(0, a.x, a.y, a.z)
      pos.setXYZ(1, b.x, b.y, b.z)
      pos.needsUpdate = true
    }
    if (focusPos && focusObj && frame.cam !== 'cockpit') {
      const g = this.heightAt(focusObj.st.lon, focusObj.st.lat) ?? 0
      setLine(this.drop, focusPos, this.toWorld(focusObj.st.lon, focusObj.st.lat, g))
    } else setLine(this.drop, null, null)
    setLine(this.braa, frame.line ? focusPos : null, frame.line ? measurePos : null)

    this.renderer.render(this.scene, this.camera)
  }

  /** The drawn aircraft/weapon nearest a screen point, within 26 px. */
  pick(x: number, y: number): number | null {
    let best: { idx: number; d: number } | null = null
    for (const p of this.projected) {
      const d = Math.hypot(p.x - x, p.y - y)
      if (d < 26 && (!best || d < best.d)) best = { idx: p.idx, d }
    }
    return best?.idx ?? null
  }
}
