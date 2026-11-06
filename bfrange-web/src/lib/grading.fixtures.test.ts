import { describe, expect, it } from 'vitest'
import fx from '../../../bfprotocols/src/range/fixtures/grading.json'
import type { WeaponClass } from '../types'
import { goldenDiff } from './goldenCompare'
import {
  SCORING_DEFAULTS,
  aarGrade,
  bombQuality,
  clock,
  glideslopeCall,
  goodRadius,
  lineupCall,
  precisionQuality,
  strafeQuality,
  waveoffLimits,
} from './grading'

// The golden cases bfprotocols/src/range/fixtures.rs writes from grading.rs
// with the default ScoringCfg.
describe('grading.ts against the Rust fixtures', () => {
  it('has the engine default scoring', () => {
    expect(goldenDiff({ ...SCORING_DEFAULTS, strafe_bands: [...SCORING_DEFAULTS.strafe_bands] }, fx.scoring_defaults)).toBeNull()
  })

  it('bands bombs like grading::bomb_quality', () => {
    for (const b of fx.bomb) {
      const cls = b.class as WeaponClass
      expect(goodRadius(cls), b.class).toBe(b.good_radius_m)
      expect(bombQuality(cls, b.miss_m), `${b.class} ${b.miss_m} m`).toBe(b.expect)
    }
  })

  it('bands strafe passes like grading::strafe_quality', () => {
    for (const s of fx.strafe) expect(strafeQuality(s.accuracy_pct, s.valid), `${s.accuracy_pct}`).toBe(s.expect)
  })

  it('bands precision like grading::precision_quality', () => {
    for (const p of fx.precision) expect(precisionQuality(p.perfect_m, p.distance_m), `${p.distance_m}`).toBe(p.expect)
  })

  it('reads clocks like grading::clock', () => {
    for (const c of fx.clock) expect(clock(c.rel_deg), `${c.rel_deg}`).toBe(c.expect)
  })

  it('calls glideslope, lineup and waveoff like grading.rs', () => {
    for (const g of fx.glideslope) expect(goldenDiff(glideslopeCall(g.gse_deg), g.expect, `gse ${g.gse_deg}`)).toBeNull()
    for (const l of fx.lineup) expect(goldenDiff(lineupCall(l.lue_deg), l.expect, `lue ${l.lue_deg}`)).toBeNull()
    for (const w of fx.waveoff) expect(waveoffLimits(w.gse_deg, w.lue_deg), `${w.gse_deg}/${w.lue_deg}`).toBe(w.expect)
  })

  it('grades AAR like grading::aar_grade', () => {
    for (const a of fx.aar) expect(goldenDiff(aarGrade(a.input), a.expect, JSON.stringify(a.input))).toBeNull()
  })
})
