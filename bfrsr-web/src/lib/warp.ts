// Perspective warp for recon photos: a pinhole-camera ground projection plus
// a DLT homography that maps the photo rectangle onto that ground quad via a
// CSS `matrix3d`. Standard techniques, written from the formulas — same idea
// the reference tarps-intel-map uses for oblique captures.

import { offsetLatLon, type LatLon } from './geo'

const FOCAL_MM = 150
const FRAME_MM = 100
const FT_TO_M = 0.3048
const MAX_WARP_ATTITUDE_DEG = 45
const MIN_RAY_DOWN = 0.02

const rad = (d: number) => (d * Math.PI) / 180

// ── DLT homography ────────────────────────────────────────────────────
type Pt = { x: number; y: number }

function solveLinearSystem(rows: number[][], values: number[]): number[] | null {
  const n = values.length
  const m = rows.map((r, i) => [...r, values[i]])
  for (let col = 0; col < n; col++) {
    let pivot = col
    for (let r = col + 1; r < n; r++) {
      if (Math.abs(m[r][col]) > Math.abs(m[pivot][col])) pivot = r
    }
    if (Math.abs(m[pivot][col]) < 1e-10) return null
    ;[m[col], m[pivot]] = [m[pivot], m[col]]
    const p = m[col][col]
    for (let c = col; c <= n; c++) m[col][c] /= p
    for (let r = 0; r < n; r++) {
      if (r === col) continue
      const f = m[r][col]
      for (let c = col; c <= n; c++) m[r][c] -= f * m[col][c]
    }
  }
  return m.map(r => r[n])
}

/** 3×3 homography (8 params) taking the source rect (0,0)-(w,h) corners
 *  [TL, TR, BR, BL] to the four target points, in the same order. */
function homographyForQuad(target: Pt[], w: number, h: number): number[] | null {
  const src: Pt[] = [
    { x: 0, y: 0 }, { x: w, y: 0 }, { x: w, y: h }, { x: 0, y: h },
  ]
  const rows: number[][] = []
  const vals: number[] = []
  for (let i = 0; i < 4; i++) {
    const s = src[i], t = target[i]
    rows.push([s.x, s.y, 1, 0, 0, 0, -t.x * s.x, -t.x * s.y]); vals.push(t.x)
    rows.push([0, 0, 0, s.x, s.y, 1, -t.y * s.x, -t.y * s.y]); vals.push(t.y)
  }
  const sol = solveLinearSystem(rows, vals)
  return sol && sol.every(Number.isFinite) ? sol : null
}

/** CSS `matrix3d(...)` string warping an element's (0,0)-(w,h) box onto the
 *  four `target` pixel points [TL, TR, BR, BL]. Apply with
 *  `transform-origin: 0 0`. Returns null if the quad is degenerate. */
export function matrix3dForQuad(target: Pt[], w: number, h: number): string | null {
  const s = homographyForQuad(target, w, h)
  if (!s) return null
  const [a, b, c, d, e, f, g, hh] = s
  // column-major 4×4
  const m = [a, d, 0, g, b, e, 0, hh, 0, 0, 1, 0, c, f, 0, 1]
  return `matrix3d(${m.map(n => (Math.abs(n) < 1e-8 ? '0' : n.toFixed(8))).join(',')})`
}

// ── Pinhole ground projection ─────────────────────────────────────────
type Vec3 = { x: number; y: number; z: number }
const len = (v: Vec3) => Math.hypot(v.x, v.y, v.z)
function norm(v: Vec3): Vec3 | null {
  const l = len(v)
  return l < 1e-9 ? null : { x: v.x / l, y: v.y / l, z: v.z / l }
}
function cross(a: Vec3, b: Vec3): Vec3 {
  return { x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x }
}

export interface WarpInput {
  lat: number
  lon: number
  altFt: number
  headingDeg: number
  pitchDeg: number
  rollDeg: number
  aspect: number   // image width / height
}

/** Project a photo's 4 corners onto the ground with a pinhole model.
 *  Returns [TL, TR, BR, BL] ground corners, or null when the attitude is
 *  outside ±45° or the frame looks above the horizon — the caller then
 *  falls back to the flat rectangular footprint. */
export function warpedGroundQuad(inp: WarpInput): LatLon[] | null {
  const { lat, lon, altFt, headingDeg, pitchDeg, rollDeg, aspect } = inp
  if (Math.abs(pitchDeg) > MAX_WARP_ATTITUDE_DEG || Math.abs(rollDeg) > MAX_WARP_ATTITUDE_DEG) {
    return null
  }
  const altM = altFt * FT_TO_M
  if (!(altM > 0)) return null

  const halfW = FRAME_MM / (2 * FOCAL_MM)
  const halfH = halfW / (aspect || 1.5)

  const rightSlope = -Math.tan(rad(rollDeg))
  const forwardSlope = -Math.tan(rad(pitchDeg))
  const centerDir = norm({ x: rightSlope, y: forwardSlope, z: 1 })
  if (!centerDir) return null

  const rightAxis = norm(cross({ x: 0, y: 1, z: 0 }, centerDir)) ?? { x: 1, y: 0, z: 0 }
  const forwardAxis = norm(cross(centerDir, rightAxis))
  if (!forwardAxis) return null

  const centerRightM = rightSlope * altM
  const centerForwardM = forwardSlope * altM

  // camera-frame corners: TL, TR, BR, BL
  const uv = [
    { u: -halfW, v: halfH }, { u: halfW, v: halfH },
    { u: halfW, v: -halfH }, { u: -halfW, v: -halfH },
  ]
  const out: LatLon[] = []
  for (const { u, v } of uv) {
    const ray = {
      x: centerDir.x + u * rightAxis.x + v * forwardAxis.x,
      y: centerDir.y + u * rightAxis.y + v * forwardAxis.y,
      z: centerDir.z + u * rightAxis.z + v * forwardAxis.z,
    }
    if (ray.z <= MIN_RAY_DOWN) return null
    const scale = altM / ray.z
    const dRight = ray.x * scale - centerRightM
    const dFwd = ray.y * scale - centerForwardM
    out.push(offsetLatLon(lat, lon, headingDeg, dRight, dFwd))
  }
  return out
}
