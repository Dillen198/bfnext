import { describe, expect, it } from 'vitest'
import fx from '../../../bfprotocols/src/range/fixtures/pattern_legs.json'
import type { FieldTaxi } from '../types'
import { goldenDiff } from './goldenCompare'
import { legs, patternSummary, taxiSummary, type RelPt } from './patternLegs'

// The golden cases bfprotocols/src/range/fixtures.rs writes from
// pattern_legs.rs. Samples are [t, along, across, agl, rel, gs_kts]; taxi
// samples [t, along, across, gs_kts, x, z].
const rel = (s: number[][]): RelPt[] => s.map(([t, along, across, agl, rel, gs_kts]) => ({ t, along, across, agl, rel, gs_kts }))
const taxi = (s: number[][]) => s.map(([t, along, across, gs_kts, x, y]) => ({ t, along, across, gs_kts, x, y }))

describe('patternLegs.ts against the Rust fixtures', () => {
  for (const p of fx.patterns) {
    it(`reads the ${p.name} pattern like pattern_legs.rs`, () => {
      const s = rel(p.samples)
      expect(legs(s, p.t_touch)).toEqual(p.legs)
      expect(goldenDiff(patternSummary(s, p.t_touch), p.pattern, p.name)).toBeNull()
    })
  }

  for (const c of fx.taxi) {
    it(`summarises the ${c.name} taxi like pattern_legs.rs`, () => {
      const x = taxiSummary(taxi(c.samples), c.rwy_len, c.rwy_width, c.end as FieldTaxi['end'], c.t_touch)
      expect(goldenDiff(x, c.expect, c.name)).toBeNull()
    })
  }
})
