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
import { isWeapon, type Kind, type State } from './data'
import { familyOf, modelOf, realOf, realModel, realLength, LENGTH, MIN_PX, type Family, type Real } from './models'
import { SHIP_FIT, waterlineZ } from '../../../shared/models/catalog'
import { drawHud, type HudData, type Vec3 } from './hud'

export type Cam3D = 'free' | 'follow' | 'chase' | 'cockpit' | 'padlock' | 'satellite'

export interface TheaterObject {
  idx: number
  kind: Kind
  name?: string | null
  color: string
  st: State
  focus: boolean
  target?: boolean
}

export interface TheaterFrame {
  objects: TheaterObject[]
  trails: { pts: [number, number, number][]; color: string; opacity: number }[]
  /** Tacview's smoke ribbons: [lon, lat, alt, roll] along the path */
  ribbons: { pts: [number, number, number, number][]; color: string; opacity: number }[]
  /** radar locks, shooter -> target (object indices) */
  locks: { a: number; b: number; color: string }[]
  /** range rings / threat domes round an object, metres */
  rings: { idx: number; r: number; color: string; opacity: number; dome: boolean }[]
  /** radar sweeps: azimuth/elevation (deg, world), range m, beam widths deg */
  cones: { idx: number; az: number; el: number; range: number; hbw: number; vbw: number; color: string }[]
  /** velocity vectors, [lon, lat, alt] now -> ahead */
  vectors: { a: [number, number, number]; b: [number, number, number]; color: string }[]
  /** a line from each aircraft down to the ground */
  verticals: boolean
  /** object size multiplier */
  scale: number
  /** the in-game clock (unix ms) the sun and sky follow; null: noon */
  sunMs: number | null
  /** BRAA line, focus -> measured */
  line: [number, number, number][] | null
  focus: number | null
  measure: number | null
  cam: Cam3D
  /** the cockpit camera's HUD (null: none) */
  hud?: HudData | null
}

export interface Projected { idx: number; x: number; y: number }

const R_EARTH = 6_371_000
const EARTH_C = 40_075_016.686
const DEG = Math.PI / 180
const FOCUS_HEX = '#b6f04a'

/** The attitude to draw. DCS writes a bomb's attitude once, at release, and
 *  many missiles' only now and then, so a weapon in flight points along its
 *  path instead. */
function drawnState(kind: Kind, st: State): State {
  const p = isWeapon(kind) && st.gs + Math.abs(st.vs) > 30 ? st.path : undefined
  return p ? { ...st, pitch: p.pitch, hdg: p.trk } : st
}
// Elevation: Mapterhorn (Copernicus GLO-30 worldwide, lidar where open data
// has it; 512 px Terrarium WebP), falling back to the AWS/Mapzen Terrarium set.
const MAPTERHORN = (z: number, x: number, y: number) => `https://tiles.mapterhorn.com/${z}/${x}/${y}.webp`
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

interface Dem { z: number; x: number; y: number; n: number; h: Float32Array } // n x n metres

async function loadDem(z: number, x: number, y: number): Promise<Dem> {
  let img: HTMLImageElement
  try { img = await loadImage(MAPTERHORN(z, x, y)) } catch { img = await loadImage(TERRARIUM(z, x, y)) }
  const n = img.naturalWidth || 256
  const cv = document.createElement('canvas')
  cv.width = cv.height = n
  const ctx = cv.getContext('2d', { willReadFrequently: true })!
  ctx.drawImage(img, 0, 0)
  const d = ctx.getImageData(0, 0, n, n).data
  const h = new Float32Array(n * n)
  for (let i = 0; i < h.length; i++) {
    // terrarium: (R*256 + G + B/256) - 32768. The sea floor is not DCS's
    // sea, which is flat at 0.
    h[i] = Math.max(0, d[i * 4] * 256 + d[i * 4 + 1] + d[i * 4 + 2] / 256 - 32768)
  }
  return { z, x, y, n, h }
}

function demAt(dem: Dem, lon: number, lat: number): number {
  const n = dem.n
  const fx = (lon2tx(lon, dem.z) - dem.x) * (n - 1)
  const fy = (lat2ty(lat, dem.z) - dem.y) * (n - 1)
  const x0 = Math.max(0, Math.min(n - 2, Math.floor(fx))), y0 = Math.max(0, Math.min(n - 2, Math.floor(fy)))
  const ax = Math.min(1, Math.max(0, fx - x0)), ay = Math.min(1, Math.max(0, fy - y0))
  const h = dem.h
  const a = h[y0 * n + x0], b = h[y0 * n + x0 + 1], c = h[(y0 + 1) * n + x0], d = h[(y0 + 1) * n + x0 + 1]
  return (a * (1 - ax) + b * ax) * (1 - ay) + (c * (1 - ax) + d * ax) * ay
}

/** The sun's azimuth (deg from north, clockwise) and elevation (deg) seen
 *  from lat/lon at a unix time: the usual low-precision almanac formulas,
 *  good to a fraction of a degree, plenty for lighting a scene. */
export function sunPosition(ms: number, lat: number, lon: number): { az: number; el: number } {
  const d = ms / 86_400_000 + 2440587.5 - 2451545.0
  const g = (357.529 + 0.98560028 * d) * DEG
  const q = 280.459 + 0.98564736 * d
  const L = (q + 1.915 * Math.sin(g) + 0.02 * Math.sin(2 * g)) * DEG
  const e = (23.439 - 0.00000036 * d) * DEG
  const ra = Math.atan2(Math.cos(e) * Math.sin(L), Math.cos(L))
  const dec = Math.asin(Math.sin(e) * Math.sin(L))
  const gmst = ((18.697374558 + 24.06570982441908 * d) % 24) * 15 * DEG
  const H = gmst + lon * DEG - ra
  const la = lat * DEG
  const el = Math.asin(Math.sin(la) * Math.sin(dec) + Math.cos(la) * Math.cos(dec) * Math.cos(H))
  const az = Math.atan2(-Math.sin(H), Math.tan(dec) * Math.cos(la) - Math.sin(la) * Math.cos(H))
  return { az: ((az / DEG) + 360) % 360, el: el / DEG }
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
  private ribbonMeshes: THREE.Mesh[] = []
  private ringLines: THREE.LineLoop[] = []
  private domes: THREE.Mesh[] = []
  private cones: THREE.Mesh[] = []
  private lockLines: THREE.LineSegments
  private sky: THREE.Mesh
  private fx: Record<'flare' | 'chaff' | 'shell', THREE.Points>
  private booms: THREE.InstancedMesh
  private satDist = 4000
  private sun: THREE.DirectionalLight
  private hemi: THREE.HemisphereLight
  private sunKey = ''
  private vectorLines: THREE.LineSegments
  private verticalLines: THREE.LineSegments
  private drop: THREE.Line
  private braa: THREE.Line
  private frameNo = 0
  private lastFocus: THREE.Vector3 | null = null
  private chaseDist = 120
  private padlockDist = 180
  private tmpM = new THREE.Matrix4()
  private tmpV = new THREE.Vector3()
  private tmpQ = new THREE.Quaternion()
  private tmpE = new THREE.Euler(0, 0, 0, 'ZXY')
  private camFix = new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3(1, 0, 0), Math.PI / 2)
  projected: Projected[] = []
  /** the camera moved by the user (orbit, zoom): a frame is needed */
  onChange: (() => void) | null = null
  /** the user dragged the view while following: hand over to free */
  onUserMove: (() => void) | null = null
  private disposed = false
  private hudCanvas: HTMLCanvasElement | null = null
  private hudOn = false

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

    // A sky dome rather than a flat colour: deep blue overhead, haze at the
    // horizon (the fog fades distant terrain into the same haze).
    const horizon = new THREE.Color('#b8cadb')
    this.scene.background = horizon
    this.scene.fog = new THREE.Fog(horizon, 60_000, 400_000)
    this.sky = new THREE.Mesh(
      new THREE.SphereGeometry(1, 32, 16),
      new THREE.ShaderMaterial({
        side: THREE.BackSide, depthWrite: false, depthTest: false, fog: false,
        uniforms: {
          top: { value: new THREE.Color('#1f4f8f') },
          horizon: { value: horizon },
          ground: { value: new THREE.Color('#7d8a8f') },
        },
        vertexShader: 'varying vec3 vDir; void main() { vDir = normalize(position); gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }',
        fragmentShader: 'uniform vec3 top; uniform vec3 horizon; uniform vec3 ground; varying vec3 vDir; void main() { float h = vDir.z; vec3 c = h > 0.0 ? mix(horizon, top, pow(clamp(h * 1.6, 0.0, 1.0), 0.6)) : mix(horizon, ground, clamp(-h * 8.0, 0.0, 1.0)); gl_FragColor = vec4(c, 1.0); }',
      }),
    )
    this.sky.renderOrder = -100
    this.sky.frustumCulled = false
    this.scene.add(this.sky)

    // Effects: flares and chaff as points of light, tracers, explosions.
    const points = (color: string, size: number, additive: boolean) => {
      const g = new THREE.BufferGeometry().setAttribute('position', new THREE.BufferAttribute(new Float32Array(3 * 256), 3))
      const p = new THREE.Points(g, new THREE.PointsMaterial({
        color, size, sizeAttenuation: false, transparent: true, depthWrite: false,
        blending: additive ? THREE.AdditiveBlending : THREE.NormalBlending,
      }))
      p.frustumCulled = false
      p.renderOrder = 5
      this.scene.add(p)
      return p
    }
    this.fx = { flare: points('#fff1c8', 6, true), chaff: points('#c9ced6', 3, false), shell: points('#ffcf5a', 3, true) }
    this.booms = new THREE.InstancedMesh(
      new THREE.SphereGeometry(1, 14, 10),
      new THREE.MeshBasicMaterial({ color: '#ff9a3c', transparent: true, opacity: 0.45, depthWrite: false }),
      64,
    )
    this.booms.instanceMatrix.setUsage(THREE.DynamicDrawUsage)
    this.booms.frustumCulled = false
    this.booms.count = 0
    this.scene.add(this.booms)

    this.lockLines = new THREE.LineSegments(
      new THREE.BufferGeometry()
        .setAttribute('position', new THREE.BufferAttribute(new Float32Array(6 * 32), 3))
        .setAttribute('color', new THREE.BufferAttribute(new Float32Array(6 * 32), 3)),
      new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity: 0.85, depthTest: false }),
    )
    this.lockLines.frustumCulled = false
    this.lockLines.renderOrder = 9
    this.scene.add(this.lockLines)
    const segs = (opacity: number) => {
      const l = new THREE.LineSegments(
        new THREE.BufferGeometry()
          .setAttribute('position', new THREE.BufferAttribute(new Float32Array(6 * 64), 3))
          .setAttribute('color', new THREE.BufferAttribute(new Float32Array(6 * 64), 3)),
        new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity }),
      )
      l.frustumCulled = false
      l.renderOrder = 8
      this.scene.add(l)
      return l
    }
    this.vectorLines = segs(0.8)
    this.verticalLines = segs(0.45)
    this.hemi = new THREE.HemisphereLight(0xdfeeff, 0x4a4030, 1.6)
    this.scene.add(this.hemi)
    this.sun = new THREE.DirectionalLight(0xfff4e0, 1.8)
    this.sun.position.set(0.5, -0.4, 1)
    this.scene.add(this.sun)
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
      this.satDist = Math.min(400_000, Math.max(300, this.satDist * f))
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
    this.hudCanvas?.remove()
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

  /** How far (model metres, before scaling) to raise a model so it sits
   *  right: a ground unit on its lowest point, a ship at its waterline.
   *  Aircraft and weapons fly where the recording puts them. */
  private seat(kind: Kind, shapeKey: string, geo: THREE.BufferGeometry): number {
    const ground = kind === 'sam' || kind === 'armor' || kind === 'vehicle' || kind === 'infantry' || kind === 'static' || kind === 'misc' || kind === 'parachutist'
    const ship = kind === 'ship' || kind === 'carrier'
    if (!ground && !ship) return 0
    if (!geo.boundingBox) geo.computeBoundingBox()
    const b = geo.boundingBox!
    if (ground) return -b.min.z
    const r = shapeKey.startsWith('real:') ? shapeKey.slice(5) : null
    const fit = r ? SHIP_FIT[r] : undefined
    // carriers: the deck about 20 m above the water; escorts: their draft;
    // anything else: a third of the way up the hull to the main deck
    const wl = r && fit ? (kind === 'carrier' ? waterlineZ(r, 20) : waterlineZ(r)) ?? fit.bottom_z + (fit.deck_z - fit.bottom_z) * 0.35 : b.min.z + (b.max.z - b.min.z) * 0.3
    return -wl
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
      this.camera.up.set(0, 0, 1)
      return
    }
    if (frame.cam !== 'satellite') cam.up.set(0, 0, 1)
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
    if (frame.cam === 'satellite') {
      // Straight down from above, north up.
      cam.position.copy(focusPos).add(new THREE.Vector3(0, -1, this.satDist))
      cam.up.set(0, 1, 0)
      cam.lookAt(focusPos)
      return
    }
    cam.up.set(0, 0, 1)
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

    // Positions first (the camera follows the focus). Effects (flares,
    // chaff, tracers, explosions) are drawn as light, not models.
    const placed: { o: TheaterObject; p: THREE.Vector3; f: Family; key: string; geo: THREE.BufferGeometry; length: number; ground: number | null }[] = []
    const effects: { o: TheaterObject; p: THREE.Vector3 }[] = []
    const where = new Map<number, THREE.Vector3>()
    let focusPos: THREE.Vector3 | null = null
    let measurePos: THREE.Vector3 | null = null
    for (const o of frame.objects) {
      const effect = o.kind === 'flare' || o.kind === 'chaff' || o.kind === 'decoy' || o.kind === 'shell' || o.kind === 'explosion'
      if (o.kind === 'bullseye') {
        where.set(o.idx, this.toWorld(o.st.lon, o.st.lat, this.heightAt(o.st.lon, o.st.lat) ?? 0, new THREE.Vector3()))
        continue
      }
      const f = familyOf(o.kind, o.name)
      const air = o.kind === 'air' || o.kind === 'helo'
      const weapon = f === 'missile' || f === 'bomb'
      let alt = o.st.alt
      const g = effect && o.kind !== 'explosion' ? null : this.heightAt(o.st.lon, o.st.lat)
      // Our terrain is the real world's, not DCS's: an aircraft near the
      // ground keeps its recorded height above DCS's ground, over ours.
      if (air && g != null && o.st.agl != null && o.st.agl < 300) {
        alt = g + o.st.agl
      } else if (air || weapon || effect) {
        if (g != null && alt < g + 1) alt = g + 1
      } else if (o.kind === 'ship' || o.kind === 'carrier') {
        alt = 0
      } else if (g != null) {
        alt = g
      }
      const p = this.toWorld(o.st.lon, o.st.lat, alt, new THREE.Vector3())
      where.set(o.idx, p)
      if (effect) { effects.push({ o, p }); continue }
      // world height of the ground under it, so no aircraft sinks into it
      const ground = air && g != null ? p.z - (alt - g) : null
      placed.push({ o, p, f, ground, ...this.shape(f, realOf(o.kind, o.name)) })
      if (o.idx === frame.focus) focusPos = p
      if (o.idx === frame.measure) measurePos = p
    }
    this.placeCamera(frame, focusPos, focusObj ? drawnState(focusObj.kind, focusObj.st) : null, measurePos)
    this.camera.updateMatrixWorld()
    const target = focusPos ?? this.controls.target
    this.updateTerrain(target)
    this.sky.position.copy(this.camera.position)
    this.sky.scale.setScalar(this.camera.far * 0.9)

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
    const project = (idx: number, p: THREE.Vector3) => {
      v.copy(p).project(this.camera)
      if (v.z < 1 && Math.abs(v.x) <= 1.1 && Math.abs(v.y) <= 1.1) {
        projected.push({ idx, x: ((v.x + 1) / 2) * w, y: ((1 - v.y) / 2) * h })
      }
    }
    for (const [key, list] of buckets) {
      const color = key.slice(key.lastIndexOf('|') + 1)
      const { f, key: shapeKey, geo, length } = list[0]
      const mesh = this.pool(shapeKey, geo, color, list.length)
      const seat = this.seat(list[0].o.kind, shapeKey, geo)
      const bottom = (geo.boundingBox ?? (geo.computeBoundingBox(), geo.boundingBox!)).min.z
      let n = 0
      for (const { o, p, ground } of list) {
        const d = this.camera.position.distanceTo(p)
        // In the cockpit, the own aircraft is not drawn over the view.
        if (frame.cam === 'cockpit' && o.focus) continue
        const scale = Math.max(1, (MIN_PX[f] * frame.scale * d) / pxPerRad / length) * Math.max(1, frame.scale / 2)
        const st = drawnState(o.kind, o.st)
        this.tmpE.set(st.pitch * DEG, st.roll * DEG, -st.hdg * DEG, 'ZXY')
        this.tmpQ.setFromEuler(this.tmpE)
        s3.set(scale, scale, scale)
        // Models are centred on their middle: stand ground units on their
        // lowest point and float ships at their waterline; an aircraft is
        // raised just enough that its wheels (at this scale) clear the ground.
        let lift = seat
        if (ground != null) lift = Math.max(0, (ground - p.z) / scale - bottom)
        if (lift) this.tmpV.copy(p).setZ(p.z + lift * scale)
        this.tmpM.compose(lift ? this.tmpV : p, this.tmpQ, s3)
        mesh.setMatrixAt(n++, this.tmpM)
        project(o.idx, p)
      }
      mesh.count = n
      mesh.instanceMatrix.needsUpdate = true
    }
    this.projected = projected

    this.drawEffects(effects, pxPerRad)

    // Trails: one line each, buffers reused frame to frame.
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
      const pos = this.buffer(l.geometry, 'position', tr.pts.length, 3)
      const arr = pos.array as Float32Array
      tr.pts.forEach(([lon, lat, alt], k) => {
        this.toWorld(lon, lat, alt, v)
        arr[k * 3] = v.x; arr[k * 3 + 1] = v.y; arr[k * 3 + 2] = v.z
      })
      pos.needsUpdate = true
      l.geometry.setDrawRange(0, tr.pts.length)
      const mat = l.material as THREE.LineBasicMaterial
      mat.color.set(tr.color)
      mat.opacity = tr.opacity
    })

    this.drawSegments(this.vectorLines, frame.vectors.map(v => {
      const a = this.toWorld(v.a[0], v.a[1], v.a[2], new THREE.Vector3())
      const b = this.toWorld(v.b[0], v.b[1], v.b[2], new THREE.Vector3())
      return { a, b, color: v.color }
    }))
    this.drawSegments(this.verticalLines, frame.verticals
      ? placed.filter(x => (x.o.kind === 'air' || x.o.kind === 'helo') && !(frame.cam === 'cockpit' && x.o.focus)).map(x => ({
        a: x.p,
        b: this.toWorld(x.o.st.lon, x.o.st.lat, this.heightAt(x.o.st.lon, x.o.st.lat) ?? 0, new THREE.Vector3()),
        color: x.o.focus ? FOCUS_HEX : x.o.color,
      }))
      : [])
    this.setSun(frame.sunMs)
    this.drawRibbons(frame.ribbons, pxPerRad)
    this.drawLocks(frame.locks, where)
    this.drawRings(frame.rings, frame.objects, where)
    this.drawCones(frame.cones, where)

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
    this.drawHud(frame, w, h, measurePos)
  }

  // ── layers ──

  /** A geometry attribute with room for `n` items, grown (doubled) when short. */
  private buffer(g: THREE.BufferGeometry, name: string, n: number, size: number): THREE.BufferAttribute {
    let a = g.getAttribute(name) as THREE.BufferAttribute | undefined
    if (!a || a.count < n) {
      a = new THREE.BufferAttribute(new Float32Array(Math.max(64, n * 2) * size), size)
      a.setUsage(THREE.DynamicDrawUsage)
      g.setAttribute(name, a)
    }
    return a
  }

  private drawEffects(effects: { o: TheaterObject; p: THREE.Vector3 }[], pxPerRad: number) {
    const groups: Record<'flare' | 'chaff' | 'shell', THREE.Vector3[]> = { flare: [], chaff: [], shell: [] }
    let nb = 0
    for (const { o, p } of effects) {
      if (o.kind === 'explosion') {
        if (nb >= this.booms.instanceMatrix.count) continue
        const d = this.camera.position.distanceTo(p)
        const r = Math.max(o.st.ch?.Radius ?? 12, (4 * d) / pxPerRad)
        this.tmpM.makeScale(r, r, r).setPosition(p)
        this.booms.setMatrixAt(nb++, this.tmpM)
      } else {
        groups[o.kind === 'flare' ? 'flare' : o.kind === 'shell' ? 'shell' : 'chaff'].push(p)
      }
    }
    this.booms.count = nb
    this.booms.instanceMatrix.needsUpdate = true
    for (const k of ['flare', 'chaff', 'shell'] as const) {
      const pts = groups[k]
      const g = this.fx[k].geometry
      const pos = this.buffer(g, 'position', pts.length, 3)
      pts.forEach((q, i) => pos.setXYZ(i, q.x, q.y, q.z))
      pos.needsUpdate = true
      g.setDrawRange(0, pts.length)
    }
  }

  /**
   * Tacview's ribbons: a band along the flight path as wide as the wings,
   * banked as the aircraft was, fading towards the tail. Kept a few pixels
   * wide however far away, so a turn fight reads from a distance.
   */
  private drawRibbons(ribbons: TheaterFrame['ribbons'], pxPerRad: number) {
    while (this.ribbonMeshes.length < ribbons.length) {
      const m = new THREE.Mesh(new THREE.BufferGeometry(), new THREE.MeshBasicMaterial({
        vertexColors: true, transparent: true, side: THREE.DoubleSide, depthWrite: false,
      }))
      m.frustumCulled = false
      m.renderOrder = 4
      this.ribbonMeshes.push(m)
      this.scene.add(m)
    }
    const P = new THREE.Vector3(), Q = new THREE.Vector3(), fwd = new THREE.Vector3()
    const right = new THREE.Vector3(), up = new THREE.Vector3(), wingv = new THREE.Vector3()
    const Z = new THREE.Vector3(0, 0, 1)
    const col = new THREE.Color()
    this.ribbonMeshes.forEach((m, i) => {
      const rb = ribbons[i]
      m.visible = !!rb && rb.pts.length > 1
      if (!m.visible) return
      const n = rb.pts.length
      const g = m.geometry
      const pos = this.buffer(g, 'position', n * 2, 3)
      const clr = this.buffer(g, 'color', n * 2, 4)
      col.set(rb.color)
      for (let k = 0; k < n; k++) {
        const [lon, lat, alt, roll] = rb.pts[k]
        this.toWorld(lon, lat, alt, P)
        const nb = rb.pts[Math.min(n - 1, k + 1)], pb = rb.pts[Math.max(0, k - 1)]
        this.toWorld(nb[0], nb[1], nb[2], Q)
        fwd.copy(Q)
        this.toWorld(pb[0], pb[1], pb[2], Q)
        fwd.sub(Q)
        if (fwd.lengthSq() < 1e-6) fwd.set(0, 1, 0)
        fwd.normalize()
        right.crossVectors(fwd, Z)
        if (right.lengthSq() < 1e-6) right.set(1, 0, 0)
        right.normalize()
        up.crossVectors(right, fwd)
        const r = roll * DEG
        wingv.copy(right).multiplyScalar(Math.cos(r)).addScaledVector(up, -Math.sin(r))
        const half = Math.max(5, (2.5 * this.camera.position.distanceTo(P)) / pxPerRad)
        pos.setXYZ(k * 2, P.x + wingv.x * half, P.y + wingv.y * half, P.z + wingv.z * half)
        pos.setXYZ(k * 2 + 1, P.x - wingv.x * half, P.y - wingv.y * half, P.z - wingv.z * half)
        const a = rb.opacity * Math.pow(k / (n - 1), 1.4)
        clr.setXYZW(k * 2, col.r, col.g, col.b, a)
        clr.setXYZW(k * 2 + 1, col.r, col.g, col.b, a)
      }
      pos.needsUpdate = true
      clr.needsUpdate = true
      // Triangle strip as an index: two triangles per step.
      let idx = g.getIndex()
      if (!idx || idx.count < (n - 1) * 6) {
        const cap = Math.max(64, n * 2)
        const arr = new Uint32Array(cap * 6)
        for (let k = 0; k < cap; k++) {
          const a0 = k * 2
          arr.set([a0, a0 + 1, a0 + 2, a0 + 1, a0 + 3, a0 + 2], k * 6)
        }
        idx = new THREE.BufferAttribute(arr, 1)
        g.setIndex(idx)
      }
      g.setDrawRange(0, (n - 1) * 6)
    })
  }

  private drawSegments(l: THREE.LineSegments, segs: { a: THREE.Vector3; b: THREE.Vector3; color: string }[]) {
    const g = l.geometry
    const pos = this.buffer(g, 'position', segs.length * 2, 3)
    const clr = this.buffer(g, 'color', segs.length * 2, 3)
    const c = new THREE.Color()
    segs.forEach((sg, i) => {
      c.set(sg.color)
      pos.setXYZ(i * 2, sg.a.x, sg.a.y, sg.a.z)
      pos.setXYZ(i * 2 + 1, sg.b.x, sg.b.y, sg.b.z)
      clr.setXYZ(i * 2, c.r, c.g, c.b)
      clr.setXYZ(i * 2 + 1, c.r, c.g, c.b)
    })
    pos.needsUpdate = true
    clr.needsUpdate = true
    g.setDrawRange(0, segs.length * 2)
  }

  /**
   * Put the sun where it was over the theatre at that moment, and colour the
   * sky and light for it: blue by day, amber at dusk, dark at night. Worked
   * out at most once a game minute.
   */
  private setSun(ms: number | null) {
    if (!this.origin) return
    const key = ms == null ? 'noon' : String(Math.floor(ms / 60_000))
    if (key === this.sunKey) return
    this.sunKey = key
    const { az, el } = ms == null ? { az: 160, el: 55 } : sunPosition(ms, this.origin.lat, this.origin.lon)
    const ce = Math.cos(el * DEG)
    this.sun.position.set(ce * Math.sin(az * DEG), ce * Math.cos(az * DEG), Math.sin(el * DEG))
    const day = Math.min(1, Math.max(0, (el + 4) / 14)) // 0 below -4 deg, 1 above 10 deg
    const dusk = Math.max(0, 1 - Math.abs(el - 2) / 8) // strongest just above the horizon
    const mix = (a: string, b: string, f: number) => new THREE.Color(a).lerp(new THREE.Color(b), f)
    const top = mix('#04070e', '#1f4f8f', day)
    const horizon = mix('#141b28', '#b8cadb', day).lerp(new THREE.Color('#e49a62'), dusk * 0.55)
    const u = (this.sky.material as THREE.ShaderMaterial).uniforms
    u.top.value.copy(top)
    u.horizon.value.copy(horizon)
    u.ground.value.copy(mix('#0b0e12', '#7d8a8f', day))
    ;(this.scene.fog as THREE.Fog).color.copy(horizon)
    ;(this.scene.background as THREE.Color).copy(horizon)
    this.sun.intensity = 1.9 * Math.min(1, Math.max(0, el / 8))
    this.sun.color.copy(mix('#ffb27a', '#fff4e0', Math.min(1, Math.max(0, el / 20))))
    this.hemi.intensity = 0.45 + 1.15 * day
    this.onChange?.()
  }

  private drawLocks(locks: TheaterFrame['locks'], where: Map<number, THREE.Vector3>) {
    const g = this.lockLines.geometry
    const pos = this.buffer(g, 'position', locks.length * 2, 3)
    const clr = this.buffer(g, 'color', locks.length * 2, 3)
    const c = new THREE.Color()
    let n = 0
    for (const l of locks) {
      const a = where.get(l.a), b = where.get(l.b)
      if (!a || !b) continue
      c.set(l.color)
      pos.setXYZ(n * 2, a.x, a.y, a.z)
      pos.setXYZ(n * 2 + 1, b.x, b.y, b.z)
      clr.setXYZ(n * 2, c.r, c.g, c.b)
      clr.setXYZ(n * 2 + 1, c.r, c.g, c.b)
      n++
    }
    pos.needsUpdate = true
    clr.needsUpdate = true
    g.setDrawRange(0, n * 2)
  }

  /** Rings on the ground round an object (bullseye range rings, threat
   *  circles), following the earth's curve; long-range SAMs get a dome. */
  private drawRings(rings: TheaterFrame['rings'], objects: TheaterObject[], where: Map<number, THREE.Vector3>) {
    const byIdx = new Map<number, TheaterObject>()
    for (const o of objects) byIdx.set(o.idx, o)
    const SEG = 72
    while (this.ringLines.length < rings.length) {
      const l = new THREE.LineLoop(new THREE.BufferGeometry(), new THREE.LineBasicMaterial({ transparent: true, depthWrite: false }))
      l.frustumCulled = false
      l.renderOrder = 3
      this.ringLines.push(l)
      this.scene.add(l)
    }
    const domeGeo = this.domes[0]?.geometry ?? new THREE.SphereGeometry(1, 32, 10, 0, Math.PI * 2, 0, Math.PI / 2).rotateX(Math.PI / 2)
    const wantDomes = rings.filter(r => r.dome).length
    while (this.domes.length < wantDomes) {
      const m = new THREE.Mesh(domeGeo, new THREE.MeshBasicMaterial({ transparent: true, opacity: 0.06, depthWrite: false, side: THREE.DoubleSide }))
      m.frustumCulled = false
      m.renderOrder = 2
      this.domes.push(m)
      this.scene.add(m)
    }
    let nd = 0
    const v = new THREE.Vector3()
    this.ringLines.forEach((l, i) => {
      const r = rings[i]
      const o = r ? byIdx.get(r.idx) : undefined
      const c = r ? where.get(r.idx) : undefined
      l.visible = !!(r && o && c)
      if (!r || !o || !c) return
      const pos = this.buffer(l.geometry, 'position', SEG, 3)
      const base = this.heightAt(o.st.lon, o.st.lat) ?? Math.max(0, o.st.alt)
      const mPerLon = 111_320 * Math.cos(o.st.lat * DEG)
      for (let k = 0; k < SEG; k++) {
        const a = (k / SEG) * Math.PI * 2
        this.toWorld(o.st.lon + (Math.sin(a) * r.r) / mPerLon, o.st.lat + (Math.cos(a) * r.r) / 110_540, base + 25, v)
        pos.setXYZ(k, v.x, v.y, v.z)
      }
      pos.needsUpdate = true
      l.geometry.setDrawRange(0, SEG)
      const mat = l.material as THREE.LineBasicMaterial
      mat.color.set(r.color)
      mat.opacity = r.opacity
      if (r.dome && nd < this.domes.length) {
        const d = this.domes[nd++]
        d.visible = true
        d.position.copy(c)
        d.scale.setScalar(r.r)
        ;(d.material as THREE.MeshBasicMaterial).color.set(r.color)
      }
    })
    for (let i = nd; i < this.domes.length; i++) this.domes[i].visible = false
  }

  /** Radar sweeps as translucent pyramids out of the nose. */
  private drawCones(cones: TheaterFrame['cones'], where: Map<number, THREE.Vector3>) {
    while (this.cones.length < cones.length) {
      const g = new THREE.BufferGeometry()
      g.setAttribute('position', new THREE.BufferAttribute(new Float32Array(5 * 3), 3))
      g.setIndex([0, 1, 2, 0, 2, 3, 0, 3, 4, 0, 4, 1])
      const m = new THREE.Mesh(g, new THREE.MeshBasicMaterial({ transparent: true, opacity: 0.07, depthWrite: false, side: THREE.DoubleSide }))
      m.frustumCulled = false
      m.renderOrder = 3
      this.cones.push(m)
      this.scene.add(m)
    }
    const dir = new THREE.Vector3(), rt = new THREE.Vector3(), upv = new THREE.Vector3(), c = new THREE.Vector3()
    this.cones.forEach((m, i) => {
      const cn = cones[i]
      const p = cn ? where.get(cn.idx) : undefined
      m.visible = !!(cn && p)
      if (!cn || !p) return
      const az = cn.az * DEG, el = cn.el * DEG
      dir.set(Math.sin(az) * Math.cos(el), Math.cos(az) * Math.cos(el), Math.sin(el))
      rt.set(Math.cos(az), -Math.sin(az), 0)
      upv.crossVectors(rt, dir).normalize()
      c.copy(p).addScaledVector(dir, cn.range)
      const hw = cn.range * Math.tan((cn.hbw / 2) * DEG), hh = cn.range * Math.tan((cn.vbw / 2) * DEG)
      const pos = m.geometry.getAttribute('position') as THREE.BufferAttribute
      pos.setXYZ(0, p.x, p.y, p.z)
      const corner = (k: number, sx: number, sy: number) => pos.setXYZ(k, c.x + rt.x * hw * sx + upv.x * hh * sy, c.y + rt.y * hw * sx + upv.y * hh * sy, c.z + rt.z * hw * sx + upv.z * hh * sy)
      corner(1, -1, -1); corner(2, 1, -1); corner(3, 1, 1); corner(4, -1, 1)
      pos.needsUpdate = true
      ;(m.material as THREE.MeshBasicMaterial).color.set(cn.color)
    })
  }

  // ── the HUD (cockpit camera) ──

  private drawHud(frame: TheaterFrame, w: number, h: number, measurePos: THREE.Vector3 | null) {
    const want = frame.cam === 'cockpit' && !!frame.hud
    if (!want) {
      if (this.hudOn && this.hudCanvas) {
        this.hudCanvas.style.display = 'none'
        this.hudOn = false
      }
      return
    }
    if (!this.hudCanvas) {
      const c = document.createElement('canvas')
      c.className = 'rp-hud'
      this.el.appendChild(c)
      this.hudCanvas = c
    }
    const c = this.hudCanvas
    const dpr = Math.min(window.devicePixelRatio || 1, 2)
    if (c.width !== Math.round(w * dpr) || c.height !== Math.round(h * dpr)) {
      c.width = Math.round(w * dpr)
      c.height = Math.round(h * dpr)
    }
    c.style.display = ''
    this.hudOn = true
    const ctx = c.getContext('2d')
    if (!ctx) return
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    const cam = this.camera
    const inv = cam.matrixWorldInverse
    const p = new THREE.Vector3()
    const toScreen = (q: THREE.Vector3): [number, number] | null => {
      p.copy(q).applyMatrix4(inv)
      if (p.z >= -1e-3) return null // behind the camera
      p.copy(q).project(cam)
      return [((p.x + 1) / 2) * w, ((1 - p.y) / 2) * h]
    }
    const far = new THREE.Vector3()
    const project = (d: Vec3) => toScreen(far.set(cam.position.x + d[0] * 10_000, cam.position.y + d[1] * 10_000, cam.position.z + d[2] * 10_000))
    drawHud(ctx, w, h, frame.hud!, project, measurePos ? toScreen(measurePos) : null)
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
