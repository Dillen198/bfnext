import { describe, expect, it } from 'vitest'
import type { AtmoLayer } from '../types'
import {
  G,
  KT,
  R_AIR,
  atmoAt,
  bodyFromWeapon,
  cdRef,
  groundSpeedFromTas,
  isaDensity,
  makeAtmo,
  metAt,
  simulate,
  trajectory,
  windComponents,
  type Body,
  type Launch,
} from './ballistics'

const body = (m: number, cal: number, cd: number): Body => ({ mass_kg: m, area_m2: Math.PI * (cal / 2) ** 2, cd_ref: cd })
const mk82 = body(241, 0.273, 0.3)
const launch = (z0: number, gs: number, dive: number, extra: Partial<Launch> = {}): Launch => ({
  z0_m: z0, ground_msl_m: 0, gs_mps: gs, dive_deg: dive, wind_along_mps: 0, wind_cross_mps: 0, ...extra,
})
const lay = (alt_m: number, wind_from_deg: number, wind_kts: number, temp_c: number, pressure_hpa: number): AtmoLayer =>
  ({ alt_m, wind_from_deg, wind_kts, temp_c, pressure_hpa })
/** A DCS layer at `h` m with ISA temperature and pressure (the Rust tests' `layer`). */
const layer = (h: number, from: number, kts: number): AtmoLayer => {
  const t = 288.15 - 0.0065 * h
  return lay(h, from, kts, t - 273.15, (101325 * Math.pow(t / 288.15, 5.255877)) / 100)
}

// the DCS-like profiles the Rust golden printer used
const P1 = [
  lay(22, 250, 8, 26.5, 1009.8),
  lay(500, 254, 12, 23.1, 955.2),
  lay(1000, 258, 16, 19.8, 900.4),
  lay(2000, 263, 22, 13.0, 797.9),
  lay(3000, 267, 27, 6.4, 705.6),
  lay(5000, 273, 35, -6.9, 546.0),
  lay(8000, 280, 45, -27.0, 358.1),
  lay(10000, 282, 52, -40.2, 267.4),
]
// out of order, a duplicate height, an unusable layer
const P2 = [
  lay(1500, 200, 12, 5, 850),
  lay(100, 240, 6, 18, 1000),
  lay(800, 230, 9, 12, 925),
  lay(800, 235, 10, 12.5, 924),
  lay(600, 0, 0, 15, 0),
]
const P3 = [lay(300, 45, 20, 10, 980)]

/**
 * Golden values printed by bfdb/src/range/ballistics.rs itself (compiled
 * standalone against a stub bfprotocols). If the Rust model changes, these
 * must be regenerated and the port updated to match.
 */
const GOLDEN: { name: string; b: Body; l: Launch; k: number; along: number; cross: number; range: number; tof: number }[] = [
  { name: 'mk82_level', b: mk82, l: launch(1500, 230, 0), k: 1.0, along: 3816.545396, cross: 0, range: 3816.545396, tof: 17.979833 },
  { name: 'mk82_dive30_wind', b: mk82, l: launch(1200, 240, 30, { ground_msl_m: 350, wind_along_mps: -6.5, wind_cross_mps: 4.0 }), k: 1.18,
    along: 1648.334011, cross: 1.355564, range: 1648.334568, tof: 7.216128 },
  { name: 'mk84_high', b: body(894, 0.458, 0.3), l: launch(6000, 250, 5, { ground_msl_m: 20, wind_along_mps: 10, wind_cross_mps: -8 }), k: 1.07,
    along: 7779.900589, cross: -26.352131, range: 7779.945219, tof: 34.281858 },
  { name: 'bdu33_k092', b: body(11.3, 0.102, 0.25), l: launch(900, 210, 20, { ground_msl_m: 100, wind_along_mps: 3, wind_cross_mps: 3 }), k: 0.92,
    along: 1600.84793, cross: 2.026749, range: 1600.849213, tof: 8.289017 },
  { name: 'vacuum', b: mk82, l: launch(1000, 200, 0), k: 0, along: 2856.173846, cross: 0, range: 2856.173846, tof: 14.280869 },
  // flown through a DCS atmosphere profile
  { name: 'layered_dive20', b: mk82, l: launch(2500, 230, 20, { ground_msl_m: 22, atmo: makeAtmo(P1, 135) }), k: 1.12,
    along: 3476.315176501, cross: -10.746600891, range: 3476.331787359, tof: 16.3574644 },
  { name: 'layered_mk84_high', b: body(894, 0.458, 0.3), l: launch(6000, 250, 5, { ground_msl_m: 150, atmo: makeAtmo(P1, 300) }), k: 1.07,
    along: 7679.606442192, cross: 26.408855768, range: 7679.651849831, tof: 34.295090118 },
  { name: 'layered_messy', b: body(11.3, 0.102, 0.25), l: launch(2000, 210, 10, { ground_msl_m: 50, atmo: makeAtmo(P2, 20) }), k: 0.92,
    along: 3249.820113262, cross: 1.41949019, range: 3249.820423272, tof: 18.043763685 },
  { name: 'layered_single', b: mk82, l: launch(1800, 220, 0, { atmo: makeAtmo(P3, 90) }), k: 1.0,
    along: 3978.321211514, cross: 11.622754007, range: 3978.338189545, tof: 19.733664387 },
]

describe('matches bfdb/src/range/ballistics.rs exactly', () => {
  for (const g of GOLDEN) {
    it(g.name, () => {
      const i = simulate(g.b, g.l, g.k)
      expect(i.along_m).toBeCloseTo(g.along, 4)
      expect(i.cross_m).toBeCloseTo(g.cross, 4)
      expect(i.range_m).toBeCloseTo(g.range, 4)
      expect(i.tof_s).toBeCloseTo(g.tof, 5)
    })
  }

  it('Atmo::at at the Rust sample points', () => {
    const a = makeAtmo(P2, 20)!
    const rust: [number, number, number, number][] = [
      [-100, 2.364521804975, 1.984069374465, 1.224934982427],
      [50, 2.364521804975, 1.984069374465, 1.203563742362],
      [100, 2.364521804975, 1.984069374465, 1.196523177172],
      [450, 3.187107980198, 2.149533687233, 1.162760185189],
      [800, 4.214078542722, 2.950729562222, 1.126874763113],
      [1000, 4.773864101944, 2.107663973016, 1.10863315593],
      [1500, 6.173328, 0, 1.064578697029],
      [3000, 6.173328, 0, 0.885453538378],
    ]
    for (const [h, along, cross, rho] of rust) {
      const r = atmoAt(a, h)
      expect(r.wind_along_mps).toBeCloseTo(along, 10)
      expect(r.wind_cross_mps).toBeCloseTo(cross, 10)
      expect(r.rho).toBeCloseTo(rho, 10)
    }
  })

  it('ISA density at the Rust sample points', () => {
    const rust: [number, number][] = [
      [0, 1.224999463], [5000, 0.736115462], [11000, 0.363917775],
      [15000, 0.193673941], [25000, 0.088035138], [-1000, 1.284890002],
    ]
    for (const [h, rho] of rust) expect(isaDensity(h)).toBeCloseTo(rho, 8)
  })

  it('trajectory() integrates the same numbers as simulate()', () => {
    for (const g of GOLDEN) {
      const t = trajectory(g.b, g.l, g.k)
      expect(t.impact).toEqual(simulate(g.b, g.l, g.k))
      expect(t.points[0].z).toBe(g.l.z0_m)
      expect(t.points[t.points.length - 1].z).toBe(0)
    }
  })
})

describe('the Rust unit tests, ported', () => {
  it('isa_points', () => {
    expect(Math.abs(isaDensity(0) - 1.225)).toBeLessThan(1e-3)
    expect(Math.abs(isaDensity(5000) - 0.7364)).toBeLessThan(2e-3)
    expect(Math.abs(isaDensity(11000) - 0.3639)).toBeLessThan(2e-3)
  })

  it('vacuum_matches_closed_form', () => {
    const i = simulate(mk82, launch(1000, 200, 0), 0)
    const t = Math.sqrt(2000 / G)
    expect(Math.abs(i.tof_s - t)).toBeLessThan(0.01)
    expect(Math.abs(i.range_m - 200 * t)).toBeLessThan(1.0)
    expect(Math.abs(i.cross_m)).toBeLessThan(1e-9)
  })

  it('drag_and_wind_behave', () => {
    const l = launch(3000, 230, 20)
    const r0 = simulate(mk82, l, 0.5).range_m
    const r1 = simulate(mk82, l, 1.0).range_m
    const r2 = simulate(mk82, l, 2.0).range_m
    expect(r0).toBeGreaterThan(r1)
    expect(r1).toBeGreaterThan(r2)
    expect(simulate(mk82, { ...l, wind_along_mps: 15 }, 1.0).range_m).toBeGreaterThan(r1)
    expect(simulate(mk82, { ...l, wind_cross_mps: 10 }, 1.0).cross_m).toBeGreaterThan(0)
  })

  it('cd_ref_rule', () => {
    expect(cdRef([1, 0.39, 0.38])).toBe(0.3)
    expect(cdRef([0.25])).toBe(0.25)
    expect(cdRef([])).toBe(0.3)
  })

  // (a)
  it('uniform_profile_matches_constant_model', () => {
    const [hdg, from, kts] = [75, 300, 25]
    const layers = Array.from({ length: 81 }, (_, i) => layer(i * 100, from, kts))
    const [wa, wc] = windComponents(hdg, from, kts)
    for (const [z0, ground, gs, dive] of [[1500, 200, 230, 0], [4500, 350, 250, 30], [6000, 0, 240, 10]]) {
      const flat = launch(z0, gs, dive, { ground_msl_m: ground, wind_along_mps: wa, wind_cross_mps: wc })
      const layered = { ...flat, atmo: makeAtmo(layers, hdg) }
      expect(layered.atmo).not.toBeNull()
      const i0 = simulate(mk82, flat, 1.1)
      const i1 = simulate(mk82, layered, 1.1)
      expect(Math.abs(i0.along_m - i1.along_m)).toBeLessThan(0.03)
      expect(Math.abs(i0.cross_m - i1.cross_m)).toBeLessThan(0.03)
      expect(Math.abs(i0.tof_s - i1.tof_s)).toBeLessThan(1e-3)
    }
  })

  // (b)
  it('wind_aloft_only_moves_a_high_release', () => {
    const hdg = 90
    const prof = (tail: number) => [
      ...[0, 1000, 2000].map(h => layer(h, hdg + 180, 0)),
      ...[2500, 4000, 6000, 8000].map(h => layer(h, hdg + 180, tail)),
    ]
    const [calm, windy] = [prof(0), prof(40)]
    const fly = (z0: number, layers: AtmoLayer[]) => simulate(mk82, launch(z0, 230, 0, { atmo: makeAtmo(layers, hdg) }), 1.0)
    expect(fly(1000, calm)).toEqual(fly(1000, windy))
    const hiCalm = fly(6000, calm)
    const hiWind = fly(6000, windy)
    expect(hiWind.along_m).toBeGreaterThan(hiCalm.along_m + 50)
    expect(Math.abs(hiWind.cross_m)).toBeLessThan(1e-6)
  })

  // (c)
  it('profile_clamps_outside_and_interpolates_inside', () => {
    // out of order, and the 1000 m layer has no pressure so is dropped
    const a = makeAtmo([lay(3000, 0, 30, -4.5, 701), lay(1000, 0, 0, 0, 0), lay(500, 270, 10, 15, 955)], 0)!
    expect(a.levels.length).toBe(2)
    // heading north: along = the north component, cross = the east one
    for (const h of [-200, 0, 500]) {
      const air = atmoAt(a, h)
      expect(Math.abs(air.wind_along_mps)).toBeLessThan(1e-9)
      expect(Math.abs(air.wind_cross_mps - 10 * KT)).toBeLessThan(1e-9)
      const t = 15 + 273.15
      const p = 95500 * Math.exp((-G * (h - 500)) / (R_AIR * t))
      expect(Math.abs(air.rho - p / (R_AIR * t))).toBeLessThan(1e-12)
    }
    for (const h of [3000, 4000, 12000]) {
      const air = atmoAt(a, h)
      expect(Math.abs(air.wind_along_mps + 30 * KT)).toBeLessThan(1e-9)
      expect(Math.abs(air.wind_cross_mps)).toBeLessThan(1e-9)
      const t = -4.5 + 273.15
      const p = 70100 * Math.exp((-G * (h - 3000)) / (R_AIR * t))
      expect(Math.abs(air.rho - p / (R_AIR * t))).toBeLessThan(1e-12)
    }
    const mid = atmoAt(a, 1750)
    expect(Math.abs(mid.wind_along_mps + 15 * KT)).toBeLessThan(1e-9)
    expect(Math.abs(mid.wind_cross_mps - 5 * KT)).toBeLessThan(1e-9)
    const t = (15 - 4.5) / 2 + 273.15
    expect(Math.abs(mid.rho - Math.sqrt(95500 * 70100) / (R_AIR * t))).toBeLessThan(1e-9)
    // nothing usable: the constant model
    expect(makeAtmo([lay(0, 0, 5, 15, 0)], 0)).toBeNull()
    expect(makeAtmo([], 0)).toBeNull()
    expect(makeAtmo(undefined, 0)).toBeNull()
  })

  it('a DCS profile really replaces the constant wind', () => {
    const layers = [0, 1000, 3000, 6000, 9000].map((h, i) => layer(h, 250 + 8 * i, 8 + 9 * i))
    const l = launch(3000, 230, 20, { wind_along_mps: 20, atmo: makeAtmo(layers, 135) })
    // the constant fields are ignored while a profile is present ...
    expect(simulate(mk82, l, 1.3)).toEqual(simulate(mk82, { ...l, wind_along_mps: -20 }, 1.3))
    // ... and used again without one
    expect(simulate(mk82, { ...l, atmo: null }, 1.3).range_m).not.toBeCloseTo(simulate(mk82, l, 1.3).range_m, 0)
  })

  it('from_weapon refuses unusable entries', () => {
    expect(bodyFromWeapon({ mass_kg: 0, caliber_m: 0.2 })).toBeNull()
    expect(bodyFromWeapon({ mass_kg: 241, caliber_m: 0.273, cx_coeff: [1] })!.cd_ref).toBe(0.3)
  })
})

describe('calculator helpers', () => {
  it('wind components use the same sign convention as the Rust', () => {
    const [a, c] = windComponents(0, 270, 10)
    expect(a).toBeCloseTo(0, 9)
    expect(c).toBeCloseTo(10 * 0.514444, 9) // from the left, blowing right
    const [h] = windComponents(0, 0, 10)
    expect(h).toBeCloseTo(-10 * 0.514444, 9) // headwind
  })

  it('TAS to ground speed: no wind, level', () => {
    expect(groundSpeedFromTas(200, 0, 0, 0)).toBeCloseTo(200, 9)
  })

  it('TAS to ground speed keeps |v - w| = TAS', () => {
    const [wa, wc] = [-8, 6]
    const dive = 25
    const gs = groundSpeedFromTas(220, dive, wa, wc)
    const vz = gs * Math.tan((dive * Math.PI) / 180)
    expect(Math.hypot(gs - wa, 0 - wc, vz)).toBeCloseTo(220, 9)
  })

  it('a headwind lowers the ground speed', () => {
    expect(groundSpeedFromTas(200, 0, -10, 0)).toBeCloseTo(190, 9)
  })

  it('refuses a release below the target', () => {
    expect(simulate(mk82, launch(-5, 200, 0), 1).range_m).toBe(0)
  })

  it('reads DCS winds aloft the way the model sees them', () => {
    // veering from 250 at the surface to 280 at 8 km
    const layers = [lay(0, 250, 8, 24, 1013), lay(8000, 280, 45, -28, 356)]
    const s = metAt(layers, 0)!
    expect(s.from_deg).toBeCloseTo(250, 9)
    expect(s.kts).toBeCloseTo(8, 9)
    expect(s.temp_c).toBeCloseTo(24, 9)
    expect(s.pressure_hpa).toBeCloseTo(1013, 9)
    const top = metAt(layers, 9000)!
    expect(top.from_deg).toBeCloseTo(280, 9)
    expect(top.kts).toBeCloseTo(45, 9)
    // halfway it has veered part of the way; the vector mean is a little
    // under the mean speed
    const mid = metAt(layers, 4000)!
    expect(mid.from_deg).toBeGreaterThan(260)
    expect(mid.from_deg).toBeLessThan(280)
    expect(mid.kts).toBeLessThan(26.5)
    expect(mid.kts).toBeGreaterThan(25)
    expect(mid.temp_c).toBeCloseTo(-2, 9)
    // and it is the same wind the model resolves against the track
    const air = atmoAt(makeAtmo(layers, 0)!, 4000)
    const [n, e] = windComponents(0, mid.from_deg, mid.kts)
    expect(air.wind_along_mps).toBeCloseTo(n, 9)
    expect(air.wind_cross_mps).toBeCloseTo(e, 9)
    expect(metAt([], 0)).toBeNull()
  })

  it('a calm DCS layer reads as calm', () => {
    expect(metAt([lay(0, 123, 0, 15, 1013)], 0)!.kts).toBe(0)
  })
})
