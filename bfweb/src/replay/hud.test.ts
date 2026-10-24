import { describe, expect, it } from 'vitest'
import { dirOf, flightPathDir, headingMarks, ladderRungs } from './hud'
import type { State } from './data'

const st = (o: Partial<State>): State => ({ lon: 0, lat: 0, alt: 3000, roll: 0, pitch: 0, hdg: 90, gs: 200, vs: 0, ...o })

describe('HUD geometry', () => {
  it('points directions east/north/up', () => {
    const [e, n, u] = dirOf(90, 0)
    expect(e).toBeCloseTo(1)
    expect(n).toBeCloseTo(0)
    expect(u).toBeCloseTo(0)
    expect(dirOf(0, 90)[2]).toBeCloseTo(1)
  })

  it('puts the flight path marker on the velocity, or on the climb angle until the track is in', () => {
    // crabbing: nose east, going north-east
    const v = flightPathDir(st({}), [100, 100, 0])
    expect(Math.atan2(v[0], v[1]) * 180 / Math.PI).toBeCloseTo(45)
    // no track yet: nose heading, 10 degree climb from vs / gs
    const f = flightPathDir(st({ gs: 100, vs: 100 * Math.tan(10 * Math.PI / 180) }), null)
    expect(Math.asin(f[2]) * 180 / Math.PI).toBeCloseTo(10)
    expect(Math.atan2(f[0], f[1]) * 180 / Math.PI).toBeCloseTo(90)
  })

  it('labels the heading tape on the tens, wrapping through north', () => {
    const m = headingMarks(355)
    const labels = m.filter(x => x.label).map(x => x.label)
    expect(labels).toContain('36')
    expect(labels).toContain('01')
    expect(m.find(x => x.label === '36')!.off).toBeCloseTo(5)
    expect(headingMarks(270).find(x => x.off === 0)!.label).toBe('27')
  })

  it('has a rung every 5 degrees, none at the horizon', () => {
    const r = ladderRungs()
    expect(r.some(x => x.el === 0)).toBe(false)
    expect(r.find(x => x.el === -10)!.label).toBe('10')
    expect(r.find(x => x.el === 15)!.label).toBeNull()
  })
})
