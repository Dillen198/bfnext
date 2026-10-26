// F-14B(U) JDAM launch acceptability region: an exact port of the native
// cartridge descriptor's lookupJDAMLAR / calcLARPoints (CoreMods F14/DTC/
// F-14BU_DTC.lua). The Mission Editor recomputes these on import, but the
// jet may read them from the file, so the exporter writes them too.

const ALTS = [5, 10, 20, 30, 40, 50] // kft
const MACHS = [0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2]
const TBL = [ // [alt][mach] = [Rmin nm, Rmax nm, half angle deg]
  [[0.87, 1.50, 20.00], [0.93, 1.95, 24.04], [1.07, 2.52, 23.64], [1.21, 3.32, 22.24], [1.35, 4.41, 24.72], [1.52, 5.77, 30.62], [1.75, 7.52, 32.31], [1.98, 8.95, 34.52], [2.23, 10.09, 37.22]],
  [[0.91, 2.29, 32.77], [1.07, 3.04, 35.12], [1.23, 3.74, 37.15], [1.42, 4.95, 36.65], [1.62, 6.36, 35.70], [1.81, 8.01, 34.41], [2.01, 9.48, 37.02], [2.22, 10.83, 39.82], [2.47, 12.02, 41.79]],
  [[1.71, 3.36, 43.14], [2.09, 3.91, 42.72], [1.85, 5.28, 44.44], [1.62, 6.75, 45.00], [1.47, 8.29, 44.95], [2.03, 10.02, 44.38], [2.21, 11.69, 44.39], [2.03, 13.33, 44.96], [2.25, 14.80, 45.00]],
  [[1.42, 4.99, 45.00], [1.89, 5.40, 45.00], [1.99, 6.06, 45.00], [2.08, 7.16, 45.00], [2.16, 8.89, 45.00], [2.37, 10.69, 45.00], [2.64, 12.53, 45.00], [2.95, 14.51, 45.00], [3.24, 16.34, 45.00]],
  [[1.37, 6.84, 45.00], [1.85, 7.35, 45.00], [2.21, 8.12, 45.00], [2.48, 8.88, 45.00], [2.48, 9.52, 45.00], [2.65, 10.18, 45.00], [2.97, 10.84, 45.00], [3.29, 13.53, 45.00], [3.61, 16.49, 45.00]],
  [[1.64, 10.26, 45.00], [2.12, 10.26, 45.00], [2.45, 10.26, 45.00], [2.77, 10.82, 45.00], [3.09, 13.37, 45.00], [3.32, 14.58, 45.00], [3.48, 14.58, 45.00], [3.64, 14.71, 45.00], [3.87, 15.27, 45.00]],
]

function bracket(arr: number[], v: number): [number, number, number] {
  if (v <= arr[0]) return [0, 0, 0]
  const n = arr.length
  if (v >= arr[n - 1]) return [n - 1, n - 1, 0]
  for (let i = 0; i < n - 1; i++) if (v >= arr[i] && v <= arr[i + 1]) return [i, i + 1, (v - arr[i]) / (arr[i + 1] - arr[i])]
  return [n - 1, n - 1, 0]
}

export interface Lar { lar_rmin_nmi: number; lar_rmax_nmi: number; lar_half_angle_deg: number }

export function lar(dropAltFt = 20000, dropSpdKt = 450): Lar {
  const altKft = dropAltFt / 1000
  const altFt = altKft * 1000
  const T = altFt < 36089 ? 288.15 - 0.001981 * altFt : 216.65 // ISA, K
  const mach = dropSpdKt / (38.967 * Math.sqrt(T))
  const [a1, a2, at] = bracket(ALTS, altKft)
  const [m1, m2, mt] = bracket(MACHS, mach)
  const f = (k: number) => {
    const im = (ai: number) => TBL[ai][m1][k] * (1 - mt) + TBL[ai][m2][k] * mt
    return im(a1) * (1 - at) + im(a2) * at
  }
  return { lar_rmin_nmi: f(0), lar_rmax_nmi: f(1), lar_half_angle_deg: f(2) }
}

/** The LAR's centre, the default launch point: midway between Rmin and
 *  Rmax on the reciprocal of the attack heading (map grid, x north). */
export function larCentre(x: number, y: number, attackHeadingDeg: number, l: Lar): { x: number; y: number } {
  const c = (((attackHeadingDeg + 180) % 360) * Math.PI) / 180
  const mid = ((l.lar_rmin_nmi + l.lar_rmax_nmi) / 2) * 1852
  return { x: x + mid * Math.cos(c), y: y + mid * Math.sin(c) }
}

/** The LAR outline, 11 vertices in map metres (port of calcLARPoints): the
 *  inner arc at Rmin, the right edge, the outer arc at Rmax back to the left,
 *  around the reciprocal of the attack heading (map grid, x north, y east). */
export function larPolygon(x: number, y: number, attackHeadingDeg: number, l: Lar): { x: number; y: number }[] {
  const rad = (d: number) => (d * Math.PI) / 180
  const rMin = l.lar_rmin_nmi * 1852
  const rMax = l.lar_rmax_nmi * 1852
  const c = rad((attackHeadingDeg + 180) % 360)
  const L = c - rad(l.lar_half_angle_deg)
  const R = c + rad(l.lar_half_angle_deg)
  const P = (r: number, a: number) => ({ x: x + r * Math.cos(a), y: y + r * Math.sin(a) })
  const pts: { x: number; y: number }[] = []
  for (let i = 0; i <= 3; i++) pts.push(P(rMin, L + ((R - L) / 3) * i))
  pts.push(P(rMax, R))
  for (let i = 5; i >= 0; i--) pts.push(P(rMax, L + ((R - L) / 6) * i))
  return pts
}
