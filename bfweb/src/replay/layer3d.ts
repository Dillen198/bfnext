// The replay's 3D view: a three.js custom layer drawn into MapLibre's own GL
// context, sharing its depth buffer so aircraft disappear behind terrain.
//
// Scene space is metres east/north/up of an origin that moves with the map
// centre every frame (float32 on the GPU cannot hold mercator coordinates to
// the metre). Models face +Y (north) with Z up.
//
// With terrain on, MapLibre's world z = 0 is the elevation under the map
// centre (`transform.elevation`), not sea level -- `queryTerrainElevation`
// returns elevation minus that for exactly this reason -- so altitudes are
// shifted by it too.

import * as THREE from 'three'
import { mergeGeometries } from 'three/examples/jsm/utils/BufferGeometryUtils.js'
import type { CustomLayerInterface, Map as MLMap } from 'maplibre-gl'
import type { Kind, State } from './data'

export interface Drawn3D {
  idx: number
  kind: Kind
  color: string
  st: State
  focus: boolean
}

export interface Trail3D {
  pts: [number, number, number][]
  color: string
  opacity: number
}

export interface Projected {
  idx: number
  x: number
  y: number
}

const EARTH_C = 40075016.686
const DEG = Math.PI / 180

function mercX(lon: number) { return (180 + lon) / 360 }
function mercY(lat: number) { return (180 - (180 / Math.PI) * Math.log(Math.tan(Math.PI / 4 + (lat * Math.PI) / 360))) / 360 }

/** Approximate real length of each model, for the minimum on-screen size. */
const LENGTH: Record<Kind, number> = {
  air: 17, helo: 18, missile: 4, bomb: 2.5, rocket: 3, torpedo: 5,
  sam: 7, armor: 7, vehicle: 6, infantry: 1.8, static: 8, ship: 120, carrier: 330,
}
const MIN_PX: Record<Kind, number> = {
  air: 30, helo: 26, missile: 10, bomb: 8, rocket: 8, torpedo: 10,
  sam: 12, armor: 10, vehicle: 10, infantry: 5, static: 8, ship: 22, carrier: 30,
}

function box(w: number, l: number, h: number, x = 0, y = 0, z = 0) {
  return new THREE.BoxGeometry(w, l, h).translate(x, y, z)
}
function tube(r1: number, r2: number, l: number, y = 0, z = 0) {
  return new THREE.CylinderGeometry(r1, r2, l, 10).translate(0, y, z)
}

function buildModels(): Record<Kind, THREE.BufferGeometry> {
  const jet = mergeGeometries([
    tube(0.75, 0.55, 13, -1, 0),
    new THREE.ConeGeometry(0.75, 4, 10).translate(0, 7.5, 0),
    // swept wing as a flattened, sheared box
    box(10, 3.6, 0.25, 0, -1.2, 0),
    box(4.6, 1.8, 0.2, 0, -6.4, 0.1),
    box(0.2, 2.6, 2.8, 0, -6.2, 1.5),
    box(1.0, 2.0, 0.9, 0, 3.2, 0.7), // canopy
  ])!
  const helo = mergeGeometries([
    box(2.4, 6.5, 2.4, 0, 1.5, 0),
    box(0.5, 7.5, 0.5, 0, -5.5, 0.4),
    box(0.2, 1.4, 2.0, 0, -9.0, 1.1),
    new THREE.CylinderGeometry(7.5, 7.5, 0.12, 24).rotateX(Math.PI / 2).translate(0, 1.5, 1.9),
  ])!
  const missile = mergeGeometries([
    tube(0.18, 0.18, 3.6, 0, 0),
    new THREE.ConeGeometry(0.18, 0.6, 8).translate(0, 2.1, 0),
    box(1.0, 0.4, 0.05, 0, -1.6, 0),
    box(0.05, 0.4, 1.0, 0, -1.6, 0),
  ])!
  const bomb = mergeGeometries([
    tube(0.25, 0.25, 2.0, 0, 0),
    new THREE.ConeGeometry(0.25, 0.6, 8).translate(0, 1.3, 0),
    box(0.8, 0.3, 0.05, 0, -1.0, 0),
    box(0.05, 0.3, 0.8, 0, -1.0, 0),
  ])!
  const vehicle = box(2.6, 6, 2.2, 0, 0, 1.1)
  const armor = mergeGeometries([box(3.4, 7, 1.6, 0, 0, 0.8), box(2.2, 3, 1.0, 0, -0.3, 2.1), box(0.3, 4, 0.3, 0, 3, 2.1)])!
  const sam = mergeGeometries([box(3, 7, 1.8, 0, 0, 0.9), box(1.8, 5, 1.6, 0, -0.5, 2.5).rotateX(0.5).translate(0, 0, 0.6)])!
  const infantry = box(0.6, 0.6, 1.8, 0, 0, 0.9)
  const statik = box(8, 8, 5, 0, 0, 2.5)
  const ship = mergeGeometries([box(15, 120, 9, 0, 0, 2), box(9, 22, 12, 0, -10, 10)])!
  const carrier = mergeGeometries([box(38, 330, 18, 0, 0, 7), box(8, 30, 22, 14, -20, 24)])!
  return {
    air: jet, helo, missile, bomb, rocket: missile, torpedo: missile,
    sam, armor, vehicle, infantry, static: statik, ship, carrier,
  }
}

export class Layer3D implements CustomLayerInterface {
  id = 'replay-3d'
  type = 'custom' as const
  renderingMode = '3d' as const

  /** What to draw; set by the engine before each repaint. */
  objects: Drawn3D[] = []
  trails: Trail3D[] = []
  /** Screen position of every drawn aircraft/weapon, after a render. */
  projected: Projected[] = []
  /** Ground-clamped z cache for slow movers: idx -> [lon, lat, z]. */
  private groundZ = new Map<number, [number, number, number]>()

  private map!: MLMap
  private renderer!: THREE.WebGLRenderer
  private scene = new THREE.Scene()
  private camera = new THREE.Camera()
  private models = buildModels()
  private pools = new Map<string, THREE.InstancedMesh>()
  private lines: THREE.Line[] = []
  private dropLine!: THREE.Line
  private tmp = new THREE.Matrix4()
  private euler = new THREE.Euler(0, 0, 0, 'ZXY')
  private q = new THREE.Quaternion()
  private v = new THREE.Vector3()
  private sv = new THREE.Vector3()

  onAdd(map: MLMap, gl: WebGLRenderingContext | WebGL2RenderingContext) {
    this.map = map
    this.renderer = new THREE.WebGLRenderer({ canvas: map.getCanvas(), context: gl, antialias: true })
    this.renderer.autoClear = false
    this.scene.add(new THREE.AmbientLight(0xffffff, 1.1))
    const sun = new THREE.DirectionalLight(0xffffff, 1.6)
    sun.position.set(0.4, -0.6, 1)
    this.scene.add(sun)
    const dl = new THREE.BufferGeometry().setAttribute('position', new THREE.Float32BufferAttribute(new Float32Array(6), 3))
    this.dropLine = new THREE.Line(dl, new THREE.LineBasicMaterial({ color: 0xb6f04a, transparent: true, opacity: 0.7 }))
    this.dropLine.frustumCulled = false
    this.scene.add(this.dropLine)
  }

  onRemove() {
    for (const m of this.pools.values()) { m.geometry.dispose(); (m.material as THREE.Material).dispose() }
    this.pools.clear()
    this.renderer?.dispose()
  }

  private pool(kind: Kind, color: string, need: number): THREE.InstancedMesh {
    const key = `${kind}|${color}`
    let m = this.pools.get(key)
    if (!m || m.instanceMatrix.count < need) {
      const cap = Math.max(16, need * 2)
      if (m) { this.scene.remove(m); (m.material as THREE.Material).dispose() }
      m = new THREE.InstancedMesh(this.models[kind], new THREE.MeshLambertMaterial({ color }), cap)
      m.instanceMatrix.setUsage(THREE.DynamicDrawUsage)
      m.frustumCulled = false
      this.pools.set(key, m)
      this.scene.add(m)
    }
    return m
  }

  /** Terrain under a point, relative to world z = 0 (see header). */
  private terrainZ(lon: number, lat: number): number | null {
    return this.map.queryTerrainElevation([lon, lat])
  }

  render(_gl: WebGLRenderingContext | WebGL2RenderingContext, matrix: unknown) {
    const map = this.map
    const tr = (map as unknown as { transform: { elevation: number; zoom: number } }).transform
    const center = map.getCenter()
    const ox = mercX(center.lng), oy = mercY(center.lat)
    const s = 1 / (EARTH_C * Math.cos(center.lat * DEG)) // mercator units per metre at the origin
    const elev0 = tr.elevation ?? 0
    const mpp = (EARTH_C * Math.cos(center.lat * DEG)) / (512 * Math.pow(2, map.getZoom()))

    const toScene = (lon: number, lat: number, zRel: number, out: THREE.Vector3) => {
      const sObj = 1 / (EARTH_C * Math.cos(lat * DEG))
      return out.set((mercX(lon) - ox) / s, -(mercY(lat) - oy) / s, (zRel * sObj) / s)
    }

    const mvp = new THREE.Matrix4().fromArray(matrix as number[])
      .multiply(new THREE.Matrix4().makeTranslation(ox, oy, 0))
      .multiply(new THREE.Matrix4().makeScale(s, -s, s))
    this.camera.projectionMatrix = mvp

    // Bucket objects by model and colour.
    const buckets = new Map<string, Drawn3D[]>()
    for (const d of this.objects) {
      const key = `${d.kind}|${d.focus ? '#b6f04a' : d.color}`
      let b = buckets.get(key)
      if (!b) buckets.set(key, (b = []))
      b.push(d)
    }
    for (const m of this.pools.values()) m.count = 0

    const canvas = map.getCanvas()
    const w = canvas.clientWidth, h = canvas.clientHeight
    const projected: Projected[] = []
    let focusPos: THREE.Vector3 | null = null
    let focusGround = 0

    for (const [key, list] of buckets) {
      const [kind, color] = key.split('|') as [Kind, string]
      const mesh = this.pool(kind, color, list.length)
      const scale = Math.max(1, (MIN_PX[kind] * mpp) / LENGTH[kind])
      let n = 0
      for (const d of list) {
        const { st } = d
        const air = kind === 'air' || kind === 'helo'
        const weapon = kind === 'missile' || kind === 'bomb' || kind === 'rocket' || kind === 'torpedo'
        let zRel: number
        if (air || weapon) {
          const ground = this.terrainZ(st.lon, st.lat)
          zRel = st.alt - elev0
          if (ground != null && zRel < ground + 1) zRel = ground + 1
          if (d.focus) focusGround = toScene(st.lon, st.lat, ground ?? -elev0, new THREE.Vector3()).z
        } else {
          // Slow movers sit on the terrain we draw, not DCS's slightly
          // different one; cached until they move ~20 m.
          const c = this.groundZ.get(d.idx)
          if (c && Math.abs(c[0] - st.lon) < 2e-4 && Math.abs(c[1] - st.lat) < 2e-4) zRel = c[2]
          else {
            const g = this.terrainZ(st.lon, st.lat)
            zRel = g ?? st.alt - elev0
            if (g != null) this.groundZ.set(d.idx, [st.lon, st.lat, g])
          }
          if (kind === 'ship' || kind === 'carrier') zRel = Math.max(zRel, -elev0)
        }
        toScene(st.lon, st.lat, zRel, this.v)
        this.euler.set(st.pitch * DEG, st.roll * DEG, -st.hdg * DEG, 'ZXY')
        this.q.setFromEuler(this.euler)
        this.sv.set(scale, scale, scale)
        this.tmp.compose(this.v, this.q, this.sv)
        mesh.setMatrixAt(n++, this.tmp)
        if (air || weapon) {
          const p = this.v.clone().applyMatrix4(mvp)
          // applyMatrix4 divides by w; behind-camera points come out mirrored
          const wv = mvp.elements[3] * this.v.x + mvp.elements[7] * this.v.y + mvp.elements[11] * this.v.z + mvp.elements[15]
          if (wv > 0 && Math.abs(p.x) <= 1.2 && Math.abs(p.y) <= 1.2) {
            projected.push({ idx: d.idx, x: ((p.x + 1) / 2) * w, y: ((1 - p.y) / 2) * h })
          }
          if (d.focus) focusPos = this.v.clone()
        }
      }
      mesh.count = n
      mesh.instanceMatrix.needsUpdate = true
    }
    this.projected = projected

    // Trails.
    while (this.lines.length < this.trails.length) {
      const g = new THREE.BufferGeometry()
      const l = new THREE.Line(g, new THREE.LineBasicMaterial({ transparent: true }))
      l.frustumCulled = false
      this.lines.push(l)
      this.scene.add(l)
    }
    this.lines.forEach((l, i) => {
      const tr3 = this.trails[i]
      l.visible = !!tr3 && tr3.pts.length > 1
      if (!l.visible) return
      const arr = new Float32Array(tr3.pts.length * 3)
      tr3.pts.forEach(([lon, lat, alt], k) => {
        toScene(lon, lat, alt - elev0, this.v)
        arr[k * 3] = this.v.x; arr[k * 3 + 1] = this.v.y; arr[k * 3 + 2] = this.v.z
      })
      l.geometry.setAttribute('position', new THREE.BufferAttribute(arr, 3))
      l.geometry.setDrawRange(0, tr3.pts.length)
      const mat = l.material as THREE.LineBasicMaterial
      mat.color.set(tr3.color)
      mat.opacity = tr3.opacity
    })

    // A drop line from the focus aircraft to the ground reads its height.
    const fp = focusPos as THREE.Vector3 | null
    this.dropLine.visible = !!fp
    if (fp) {
      const pos = this.dropLine.geometry.getAttribute('position') as THREE.BufferAttribute
      pos.setXYZ(0, fp.x, fp.y, fp.z)
      pos.setXYZ(1, fp.x, fp.y, focusGround)
      pos.needsUpdate = true
    }

    this.renderer.resetState()
    this.renderer.render(this.scene, this.camera)
  }
}

/** Meters per pixel at the map centre. */
export function centerMpp(map: MLMap): number {
  return (EARTH_C * Math.cos(map.getCenter().lat * DEG)) / (512 * Math.pow(2, map.getZoom()))
}

