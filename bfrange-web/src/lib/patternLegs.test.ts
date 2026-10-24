import { describe, expect, it } from 'vitest'
import { legs, patternSummary, taxiSummary, type RelPt } from './patternLegs'

const NM = 1852
const FT = 3.28084

/** The engine's test pattern (bfrange/src/pattern_legs.rs `overhead`): a left-hand overhead to runway north. */
function overhead(): RelPt[] {
  const s: RelPt[] = []
  let t = 0
  const push = (along: number, across: number, aglFt: number, rel: number, gs: number) => { s.push({ t, along, across, agl: aglFt / FT, rel, gs_kts: gs }); t++ }
  for (let a = -3 * NM; a < 1000; a += 150) push(a, 10, 1500, 0, 300)
  for (let k = 0; k < 12; k++) { const th = ((k + 0.5) / 12) * Math.PI; push(1000 + Math.sin(th) * 400, -(1 - Math.cos(th)) * 1.2 * NM / 2, 1500 - k * 40, -(k + 0.5) * 15, 250) }
  for (let a = 1000; a > -1200; a -= 75) push(a, -1.2 * NM, 1000, 180, 150)
  for (let k = 0; k < 10; k++) { const th = ((k + 0.5) / 10) * Math.PI; push(-1200 - Math.sin(th) * 350, -1.2 * NM * Math.max(Math.max(Math.cos(th), 0), (1 - th / Math.PI) * 0.6), 900 - k * 30, 180 - (k + 0.5) * 18, 140) }
  for (let a = -0.6 * NM; a < 300; a += 60) push(a, 2, Math.max(0, Math.max(0, -a) * 0.0524 * FT), 0, 135)
  const end = s[s.length - 1].t
  return s.map(x => ({ ...x, t: x.t - end }))
}

describe('pattern legs (port of the engine)', () => {
  it('reads an overhead pattern like the engine does', () => {
    const s = overhead()
    const p = patternSummary(s, 0)
    expect(p.kind).toBe('overhead')
    expect(p.side).toBe('left')
    expect(p.abeam_nm!).toBeCloseTo(1.2, 1)
    expect(p.downwind_alt_ft!).toBeCloseTo(1000, 0)
    expect(p.rollout_nm!).toBeGreaterThan(0.4)
    expect(p.calls).toEqual([])
    const lg = legs(s, 0)
    expect(lg[0]).toBe('initial')
    expect(lg.includes('downwind') && lg.includes('base') && lg[lg.length - 1] === 'final').toBe(true)
  })

  it('summarises a taxi', () => {
    const s = [] as { t: number; along: number; across: number; gs_kts: number; x: number; y: number }[]
    let t = 0, along = 300, gs = 130
    while (gs > 20) { s.push({ t, along, across: 0, gs_kts: gs, x: along, y: 0 }); along += gs * 0.514; gs -= 6; t++ }
    for (let k = 0; k < 40; k++) s.push({ t: t++, along, across: 30 + k * 8, gs_kts: k >= 10 && k < 15 ? 38 : k >= 20 && k < 26 ? 0 : 15, x: along, y: 30 + k * 8 })
    const x = taxiSummary(s, 2400, 45, 'parked')
    expect(x.max_taxi_kts).toBe(38)
    expect(x.stops).toBe(1)
    expect(x.calls.some(c => c.startsWith('fast taxi'))).toBe(true)
  })
})
