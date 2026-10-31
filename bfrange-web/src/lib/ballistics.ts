/**
 * Unguided bomb ballistics for the /calc release calculator.
 *
 * !!! THIS IS A PORT OF `bfdb/src/range/ballistics.rs` -- KEEP THEM IDENTICAL !!!
 * bfdb fits a per-weapon drag multiplier `k` (the `drag_scale` of a
 * `/api/range/weapons` calibration entry) against the drops recorded on the
 * range using ITS copy of this model. A `k` only means something inside the
 * exact model it was fitted with, so the equations, constants, integrator,
 * step, clamps and stopping rule below must match the Rust line for line.
 * `ballistics.test.ts` pins golden values printed by the Rust code; if you
 * change the model, change both files and regenerate those numbers.
 *
 * Model (3-DOF point mass, flat earth), from the Rust doc:
 *  - frame: x along the release ground track, y cross-track (+ right),
 *    z metres above the impact ground; every height is h = ground_msl + z
 *  - initial state: x = y = 0, z = z0, vx = gs, vy = 0, vz = -gs * tan(dive),
 *    dive clamped to [-80, 85] deg (+ = descending); gs is the inertial
 *    along-track ground speed
 *  - the air (wind w_along(h), w_cross(h) and density rho(h)) comes from:
 *    DCS ATMOSPHERE, when the launch carries an `Atmo` (at least one usable
 *    `AtmoLayer`: every field finite, pressure > 0, temp > -273.15 C):
 *      layers sorted by alt_m (stable); per layer to = from + 180,
 *      n = kts * 0.514444 * cos(to), e = kts * 0.514444 * sin(to),
 *      T = temp_c + 273.15, p = hPa * 100;
 *      h <= lowest / >= highest layer: that layer's n, e, T and
 *      p = p_end * exp(-g (h - h_end) / (287.053 T_end));
 *      inside: lo = highest-index layer with alt <= h, hi = lo + 1,
 *      f = (h - lo.alt) / (hi.alt - lo.alt), n, e, T linear in f,
 *      p = exp(ln p_lo + f (ln p_hi - ln p_lo));
 *      then w_along = n cos H + e sin H, w_cross = e cos H - n sin H (H the
 *      track), rho = p / (287.053 T); no height clamp
 *    CONSTANT WIND + ISA otherwise (records from older engines, and the
 *    calculator when the user types the wind in):
 *      wind W from `wind_from`: a = (wind_from + 180) - heading,
 *      w_along = W cos a, w_cross = W sin a;
 *      rho: ISA at h, h clamped to [-500, 20000]
 *  - air velocity va = v - w;
 *    accel = -(k * 0.5 * rho(h) * |va| * Cd_ref * A / m) * va + (0, 0, -g)
 *    A = pi (caliber / 2)^2; Cd_ref = cx_coeff[0] if in 0.05..=0.8 else 0.3
 *  - classic RK4, dt = 0.02 s, until z <= 0 (or 180 s); the impact is
 *    linearly interpolated inside the last step
 *  - ground range = hypot(x, y) at impact
 */
import type { AtmoLayer } from '../types'

export const G = 9.80665
export const DT = 0.02
export const KT = 0.514444
export const MAX_T = 180
export const DEFAULT_CD = 0.3
/** specific gas constant of dry air, J/(kg K) */
export const R_AIR = 287.053

/** ISA air density, kg/m³, at `h` metres MSL. Mirrors `isa_density`. */
export function isaDensity(hIn: number): number {
  const h = Math.min(20000, Math.max(-500, hIn))
  let t: number, p: number
  if (h <= 11000) {
    t = 288.15 - 0.0065 * h
    p = 101325 * Math.pow(t / 288.15, 5.255877)
  } else {
    t = 216.65
    p = 22632.06 * Math.exp(-0.000157688 * (h - 11000))
  }
  return p / (R_AIR * t)
}

// ─── DCS atmosphere (mirrors `Atmo`) ───────────────────────────────────────

interface Level {
  /** metres MSL */
  h: number
  /** wind blowing TO the north / east, m/s */
  n: number
  e: number
  /** K */
  t: number
  /** Pa, and its natural log */
  p: number
  ln_p: number
}

/** DCS's atmosphere over a drop, sorted and resolved against the track. */
export interface Atmo {
  levels: Level[]
  cos_h: number
  sin_h: number
}

/** The air at one height: wind in the track frame and density. */
export interface Air {
  wind_along_mps: number
  /** + = blowing to the right of the track */
  wind_cross_mps: number
  rho: number
}

function usable(l: AtmoLayer): boolean {
  return [l.alt_m, l.wind_from_deg, l.wind_kts, l.temp_c, l.pressure_hpa].every(v => Number.isFinite(v))
    && l.pressure_hpa > 0
    && l.temp_c > -273.15
}

/**
 * Mirrors `Atmo::new`: null when no layer is usable (an old record, or the
 * range server did not send one); the model then uses constant wind + ISA.
 */
export function makeAtmo(layers: readonly AtmoLayer[] | null | undefined, heading_deg: number): Atmo | null {
  const levels: Level[] = (layers ?? []).filter(usable).map(l => {
    const to = ((l.wind_from_deg + 180) * Math.PI) / 180
    const w = l.wind_kts * KT
    const p = l.pressure_hpa * 100
    return { h: l.alt_m, n: w * Math.cos(to), e: w * Math.sin(to), t: l.temp_c + 273.15, p, ln_p: Math.log(p) }
  })
  if (!levels.length) return null
  // Array.prototype.sort is stable, as Rust's sort_by is
  levels.sort((a, b) => a.h - b.h)
  const hdg = (heading_deg * Math.PI) / 180
  return { levels, cos_h: Math.cos(hdg), sin_h: Math.sin(hdg) }
}

/** Wind and density at `h` metres MSL. Mirrors `Atmo::at`. */
export function atmoAt(a: Atmo, h: number): Air {
  const [n, e, t, p] = sampleLevels(a, h)
  return {
    wind_along_mps: n * a.cos_h + e * a.sin_h,
    wind_cross_mps: e * a.cos_h - n * a.sin_h,
    rho: p / (R_AIR * t),
  }
}

/** [n, e] (m/s, blowing to), T (K) and p (Pa) at `h`: the first half of `Atmo::at`. */
function sampleLevels(a: Atmo, h: number): [number, number, number, number] {
  const lv = a.levels
  const first = lv[0]
  const last = lv[lv.length - 1]
  let n: number, e: number, t: number, p: number
  if (h <= first.h) {
    ;[n, e, t, p] = [first.n, first.e, first.t, first.p * Math.exp((-G * (h - first.h)) / (R_AIR * first.t))]
  } else if (h >= last.h) {
    ;[n, e, t, p] = [last.n, last.e, last.t, last.p * Math.exp((-G * (h - last.h)) / (R_AIR * last.t))]
  } else {
    // lv[lo].h <= h < lv[hi].h throughout
    let lo = 0
    let hi = lv.length - 1
    while (hi - lo > 1) {
      const mid = Math.floor((lo + hi) / 2)
      if (lv[mid].h <= h) lo = mid
      else hi = mid
    }
    const A = lv[lo]
    const B = lv[hi]
    const f = (h - A.h) / (B.h - A.h)
    n = A.n + f * (B.n - A.n)
    e = A.e + f * (B.e - A.e)
    t = A.t + f * (B.t - A.t)
    p = Math.exp(A.ln_p + f * (B.ln_p - A.ln_p))
  }
  return [n, e, t, p]
}

/** `cx_coeff[0]` when it looks like a drag coefficient, else 0.3. Mirrors `cd_ref`. */
export function cdRef(cx: number[] | undefined): number {
  const c = cx?.[0]
  return c !== undefined && c >= 0.05 && c <= 0.8 ? c : DEFAULT_CD
}

export interface Body {
  mass_kg: number
  area_m2: number
  cd_ref: number
}

/** Mirrors `Body::from_weapon`; null when mass or caliber is unusable. */
export function bodyFromWeapon(w: { mass_kg: number; caliber_m: number; cx_coeff?: number[] }): Body | null {
  if (!(w.mass_kg > 0) || !(w.caliber_m > 0)) return null
  return { mass_kg: w.mass_kg, area_m2: Math.PI * (w.caliber_m / 2) ** 2, cd_ref: cdRef(w.cx_coeff) }
}

export interface Launch {
  /** height above the impact ground, m */
  z0_m: number
  /** MSL height of the impact ground, m */
  ground_msl_m: number
  /** inertial horizontal speed along track, m/s */
  gs_mps: number
  /** flight-path dive angle, degrees, + = descending */
  dive_deg: number
  /** the constant wind, used only without `atmo` */
  wind_along_mps: number
  wind_cross_mps: number
  /** DCS's layered atmosphere; when present it replaces the constant wind and ISA */
  atmo?: Atmo | null
}

/** Wind and density at `h` metres MSL for a launch. Mirrors `Launch::air`. */
export function launchAir(l: Launch, h: number): Air {
  return l.atmo ? atmoAt(l.atmo, h) : { wind_along_mps: l.wind_along_mps, wind_cross_mps: l.wind_cross_mps, rho: isaDensity(h) }
}

export interface Impact {
  along_m: number
  cross_m: number
  range_m: number
  tof_s: number
}

type State = [number, number, number, number, number, number]

function deriv(s: State, b: Body, l: Launch, k: number): State {
  const air = launchAir(l, l.ground_msl_m + s[2])
  const va0 = s[3] - air.wind_along_mps
  const va1 = s[4] - air.wind_cross_mps
  const va2 = s[5]
  const speed = Math.sqrt(va0 * va0 + va1 * va1 + va2 * va2)
  const c = (k * 0.5 * air.rho * speed * b.cd_ref * b.area_m2) / b.mass_kg
  return [s[3], s[4], s[5], -c * va0, -c * va1, -c * va2 - G]
}

function add(a: State, b: State, h: number): State {
  return [a[0] + b[0] * h, a[1] + b[1] * h, a[2] + b[2] * h, a[3] + b[3] * h, a[4] + b[4] * h, a[5] + b[5] * h]
}

/**
 * The shared integrator. `onStep` sees every accepted state (after the first)
 * so the site can draw the trajectory without changing the numbers.
 * Returns the impact and the interpolated state at impact.
 */
function run(b: Body, l: Launch, k: number, onStep?: (t: number, s: State) => void): { impact: Impact; end: State } {
  const dive = (Math.min(85, Math.max(-80, l.dive_deg)) * Math.PI) / 180
  let s: State = [0, 0, Math.max(0, l.z0_m), l.gs_mps, 0, -l.gs_mps * Math.tan(dive)]
  let t = 0
  if (s[2] <= 0) return { impact: { along_m: 0, cross_m: 0, range_m: 0, tof_s: 0 }, end: s }
  while (t < MAX_T) {
    const k1 = deriv(s, b, l, k)
    const k2 = deriv(add(s, k1, DT / 2), b, l, k)
    const k3 = deriv(add(s, k2, DT / 2), b, l, k)
    const k4 = deriv(add(s, k3, DT), b, l, k)
    const n = [...s] as State
    for (let i = 0; i < 6; i++) n[i] += (DT / 6) * (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i])
    if (n[2] <= 0) {
      const f = s[2] / (s[2] - n[2])
      const end = s.map((v, i) => v + f * (n[i] - v)) as State
      const x = end[0]
      const y = end[1]
      return { impact: { along_m: x, cross_m: y, range_m: Math.hypot(x, y), tof_s: t + f * DT }, end }
    }
    s = n
    t += DT
    onStep?.(t, s)
  }
  return { impact: { along_m: s[0], cross_m: s[1], range_m: Math.hypot(s[0], s[1]), tof_s: t }, end: s }
}

/** Fly one bomb. `k` is the drag multiplier (0 = vacuum). Mirrors `simulate`. */
export function simulate(b: Body, l: Launch, k: number): Impact {
  return run(b, l, k).impact
}

// ─── calculator helpers (site only; the model above is unchanged) ──────────

/** What a pilot would read off a winds-aloft chart at one height. */
export interface MetAt {
  /** direction the wind blows FROM, degrees true */
  from_deg: number
  kts: number
  temp_c: number
  pressure_hpa: number
}

/**
 * DCS's wind, temperature and pressure at `h` metres MSL, interpolated
 * exactly as the model interpolates them (wind as a vector, so a veering
 * wind turns smoothly). null when there is no usable layer.
 */
export function metAt(layers: readonly AtmoLayer[] | null | undefined, h: number): MetAt | null {
  const a = makeAtmo(layers, 0)
  if (!a) return null
  const [n, e, t, p] = sampleLevels(a, h)
  const kts = Math.hypot(n, e) / KT
  const from = kts < 1e-6 ? 0 : (((Math.atan2(e, n) * 180) / Math.PI + 180) % 360 + 360) % 360
  return { from_deg: from, kts, temp_c: t - 273.15, pressure_hpa: p / 100 }
}

/** Wind components in the track frame, m/s: [along, cross(+right)]. */
export function windComponents(track_deg: number, wind_from_deg: number, wind_kts: number): [number, number] {
  const a = ((wind_from_deg + 180 - track_deg) * Math.PI) / 180
  const w = wind_kts * KT
  return [w * Math.cos(a), w * Math.sin(a)]
}

/**
 * The inertial along-track ground speed for a given true airspeed, when the
 * aircraft crabs so its ground track stays on the attack course (so vy = 0,
 * as the model assumes) and the flight-path angle over the ground is `dive`.
 * Solves |(gs, 0, -gs tan dive) - (w_along, w_cross, 0)| = tas.
 * NaN when the wind is stronger than the aircraft.
 */
export function groundSpeedFromTas(tas_mps: number, dive_deg: number, w_along: number, w_cross: number): number {
  const t = Math.tan((Math.min(85, Math.max(-80, dive_deg)) * Math.PI) / 180)
  const a = 1 + t * t
  const bh = w_along
  const c = w_along * w_along + w_cross * w_cross - tas_mps * tas_mps
  const disc = bh * bh - a * c
  if (disc < 0) return NaN
  return (bh + Math.sqrt(disc)) / a
}

export interface DropPoint {
  t: number
  /** downrange, m */
  x: number
  /** right of track, m */
  y: number
  /** height above the target, m */
  z: number
  /** inertial speed, m/s */
  v: number
}

export interface Trajectory {
  impact: Impact
  /** impact angle below the horizon (inertial velocity), degrees */
  impact_angle_deg: number
  /** inertial impact speed, m/s */
  impact_speed_mps: number
  /** state every ~0.1 s, plus release and impact */
  points: DropPoint[]
}

/** `simulate` plus the path and impact velocity, for the chart and readouts. */
export function trajectory(b: Body, l: Launch, k: number): Trajectory {
  const dive = (Math.min(85, Math.max(-80, l.dive_deg)) * Math.PI) / 180
  const v0 = Math.hypot(l.gs_mps, l.gs_mps * Math.tan(dive))
  const points: DropPoint[] = [{ t: 0, x: 0, y: 0, z: Math.max(0, l.z0_m), v: v0 }]
  let n = 0
  const { impact, end } = run(b, l, k, (t, s) => {
    if (++n % 5 === 0) points.push({ t, x: s[0], y: s[1], z: s[2], v: Math.hypot(s[3], s[4], s[5]) })
  })
  const horiz = Math.hypot(end[3], end[4])
  const speed = Math.hypot(end[3], end[4], end[5])
  points.push({ t: impact.tof_s, x: impact.along_m, y: impact.cross_m, z: 0, v: speed })
  return {
    impact,
    impact_angle_deg: (Math.atan2(-end[5], horiz) * 180) / Math.PI,
    impact_speed_mps: speed,
    points,
  }
}
