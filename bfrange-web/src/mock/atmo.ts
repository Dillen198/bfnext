/**
 * The mock mission's weather, shaped like what the engine reads from DCS
 * (`atmosphere.getWind`, `atmosphere.getTemperatureAndPressure`): a warm
 * August day over the Caucasus with the wind veering and strengthening with
 * height, 250°/8 kt at the surface to 280°/45 kt at 8 km, breathing a little
 * with the wall clock so the calculators visibly refresh.
 */
import type { AtmoLayer } from '../types'

/** (height m MSL, wind from °, wind kt) the profile passes through */
const WIND: [number, number, number][] = [
  [0, 250, 8],
  [1000, 256, 14],
  [2000, 263, 22],
  [4000, 270, 30],
  [8000, 280, 45],
  [10000, 282, 52],
]

/** the heights DCS is sampled at, above the ground */
const LEVELS = [500, 1000, 1500, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000]

const SURFACE_C = 26
const LAPSE_K_PER_M = 0.0065
const QNH_HPA = 1012.6

const r1 = (x: number) => Math.round(x * 10) / 10

/** Wind (from °, kt) at `h` m MSL and wall-clock time `t` s. */
export function mockWind(h: number, t: number): [number, number] {
  let i = 0
  while (i < WIND.length - 2 && h > WIND[i + 1][0]) i++
  const [h0, d0, s0] = WIND[i]
  const [h1, d1, s1] = WIND[i + 1]
  const f = Math.min(1, Math.max(0, (h - h0) / (h1 - h0)))
  // low-level wind wanders more than the wind aloft
  const low = Math.max(0, 1 - h / 3000)
  const dir = d0 + f * (d1 - d0) + 5 * low * Math.sin(t / 420)
  const kts = s0 + f * (s1 - s0) + 1.5 * low * Math.sin(t / 300 + 1)
  return [((dir % 360) + 360) % 360, Math.max(0, kts)]
}

/** One DCS-style layer at `h` m MSL. */
export function mockLayer(h: number, t: number): AtmoLayer {
  const [from, kts] = mockWind(h, t)
  const t0 = SURFACE_C + 273.15
  const tk = t0 - LAPSE_K_PER_M * h
  return {
    alt_m: Math.round(h),
    wind_from_deg: r1(from),
    wind_kts: r1(kts),
    temp_c: r1(tk - 273.15),
    pressure_hpa: r1(QNH_HPA * Math.pow(tk / t0, 9.80665 / (LAPSE_K_PER_M * 287.053))),
  }
}

/** DCS's atmosphere over a point whose ground is `ground_m` MSL, ground .. 10 km. */
export function mockAtmo(ground_m: number, t: number): AtmoLayer[] {
  return [ground_m, ...LEVELS.filter(h => h > ground_m + 100)].map(h => mockLayer(h, t))
}

export const MOCK_QNH_HPA = QNH_HPA
